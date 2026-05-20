{-# LANGUAGE RankNTypes #-}

module Nonogram.Knowledge.GridNST (
	GridNST,
	runGridNST,
	) where

import Control.Monad.ST (ST, runST)
import Data.Array.ST
	( STArray
	, newArray, readArray, writeArray, getAssocs, newGenArray, getBounds
	)
import qualified Data.Set as S

import Nonogram.Coordinate (Dimensions, Coordinate, minCoordinate, maxCoordinate)
import Nonogram.Knowledge (Knowledge(..))
import Nonogram.Knowledge.Class (MonadError(..), KnowledgeGrid(..), NKnowledgeGrid(..))
import Nonogram.Solution (fromFilled)

newtype GridNST x = GridNST
	( Dimensions -> forall s. STArray s Coordinate Knowledge ->
		ST s [(STArray s Coordinate Knowledge, x)]
	)

instance Functor GridNST where
	fmap f (GridNST x) =
		GridNST $ fmap (fmap $ fmap $ fmap $ fmap f) x

instance Applicative GridNST where
	pure x = GridNST $ \_ r -> pure [(r, x)]
	GridNST f <*> GridNST x = GridNST $ \d r -> do
		fs <- f d r
		concat <$> traverse
			(\(fr,ff) -> do
				xx <- x d fr
				pure $ fmap ff <$> xx
			)
			fs

instance Monad GridNST where
	GridNST x >>= f = GridNST $ \d r -> do
		xs <- x d r
		concat <$> traverse
			(\(xr,xx) -> let GridNST fx = f xx in
				fx d xr
			)
			xs

instance MonadError GridNST where
	error = GridNST $ pure $ pure $ pure []

instance KnowledgeGrid GridNST where
	readCell c = GridNST $ \_ r -> do
		k <- readArray r c
		pure [(r, k)]
	writeCell c k = GridNST $ \_ r -> do
		writeArray r c k
		pure [(r, ())]
	getUnknownOrSolution = GridNST $ \d r -> do
		assocs <- getAssocs r
		let
			result = fromFilled d <$> foldl (liftA2 (<>)) (pure mempty) (fmap
				(\(c,k) -> case k of
					Empty -> pure mempty
					Filled -> pure $ S.singleton c
					Unknown -> Left c
				)
				assocs
				)
		pure [(r, result)]

instance NKnowledgeGrid GridNST where
	tryAll c = GridNST $ \_ r -> do
		k <- readArray r c
		if k /= Unknown then
			pure [(r, ())]
		else do
			bounds <- getBounds r
			newr <- newGenArray bounds (readArray r)
			writeArray r c Empty
			writeArray newr c Filled
			pure [(r, ()), (newr, ())]

runGridNST :: GridNST x -> Dimensions -> [x]
runGridNST (GridNST f) d = runST $ do
	r <- newArray (minCoordinate, maxCoordinate d) Unknown
	threads <- f d r
	pure $ snd <$> threads