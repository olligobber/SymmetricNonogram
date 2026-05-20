{-# LANGUAGE RankNTypes #-}

module Nonogram.Knowledge.GridST (
	GridST,
	runGridST,
	) where

import Control.Monad.ST (ST, runST)
import Data.Maybe (listToMaybe)
import Data.Array.ST
	( STArray
	, newArray, readArray, writeArray, getAssocs, foldlMArray'
	)
import qualified Data.Set as S

import Nonogram.Coordinate (Dimensions, Coordinate, minCoordinate, maxCoordinate)
import Nonogram.Knowledge (Knowledge(..))
import Nonogram.Knowledge.Class (MonadRaise(..), KnowledgeGrid(..))
import Nonogram.Solution (fromFilled)

newtype GridST x = GridST
	(Dimensions -> forall s. STArray s Coordinate Knowledge -> ST s (Maybe x))

instance Functor GridST where
	fmap f (GridST x) = GridST $ fmap (fmap $ fmap $ fmap f) x

instance Applicative GridST where
	pure x = GridST $ pure $ pure $ pure $ pure x
	GridST f <*> GridST x = GridST $
		liftA2 (liftA2 $ liftA2 (<*>)) f x

instance Monad GridST where
	GridST x >>= f = GridST $ \d r -> do
		x' <- x d r
		case x' of
			Nothing -> pure Nothing
			Just x'' ->
				let
					GridST fx = f x''
				in
					fx d r

instance MonadRaise GridST where
	raise = GridST $ pure $ pure $ pure Nothing

instance KnowledgeGrid GridST where
	readCell c = GridST $ \_ r -> Just <$> readArray r c
	writeCell c k = GridST $ \_ r -> Just <$> writeArray r c k
	getUnknownOrSolution = GridST $ \d r -> do
		assocs <- getAssocs r
		pure $ Just $ fromFilled d <$> foldl (liftA2 (<>)) (pure mempty) (fmap
			(\(c,k) -> case k of
				Empty -> pure mempty
				Filled -> pure $ S.singleton c
				Unknown -> Left c
			)
			assocs
			)
	isSolved = GridST $ \_ r ->
		Just <$> foldlMArray' (\a k -> a && (k /= Unknown)) True r
	getSolution = GridST $ \d r -> do
		assocs <- getAssocs r
		pure $ Just $ fromFilled d <$> foldl (liftA2 (<>)) (pure mempty) (fmap
			(\(c,k) -> case k of
				Empty -> pure mempty
				Filled -> pure $ S.singleton c
				Unknown -> Nothing
			)
			assocs
			)
	getUnknown = GridST $ \_ r -> do
		assocs <- getAssocs r
		pure $ Just $ listToMaybe $ fmap fst $ filter ((== Unknown) . snd) assocs

runGridST :: GridST x -> Dimensions -> Maybe x
runGridST (GridST f) d = runST $ do
	r <- newArray (minCoordinate, maxCoordinate d) Unknown
	f d r

