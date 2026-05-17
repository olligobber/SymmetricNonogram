{-# LANGUAGE RankNTypes #-}

module Nonogram.Knowledge.STKnowledgeGrid (
	STKnowledgeGrid,
	runSTKnowledgeGrid,
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
import Nonogram.Knowledge.Class (MonadError(..), KnowledgeGrid(..))
import Nonogram.Solution (fromFilled)

newtype STKnowledgeGrid x =
	STKnowledgeGrid (Dimensions -> forall s. STArray s Coordinate Knowledge -> ST s (Maybe x))

instance Functor STKnowledgeGrid where
	fmap f (STKnowledgeGrid x) = STKnowledgeGrid $ fmap (fmap $ fmap $ fmap f) x

instance Applicative STKnowledgeGrid where
	pure x = STKnowledgeGrid $ pure $ pure $ pure $ pure x
	(STKnowledgeGrid f) <*> (STKnowledgeGrid x) = STKnowledgeGrid $
		liftA2 (liftA2 $ liftA2 (<*>)) f x

instance Monad STKnowledgeGrid where
	(STKnowledgeGrid x) >>= f = STKnowledgeGrid $ \d r -> do
		x' <- x d r
		case x' of
			Nothing -> pure Nothing
			Just x'' ->
				let
					STKnowledgeGrid fx = f x''
				in
					fx d r

instance MonadError STKnowledgeGrid where
	fail = STKnowledgeGrid $ pure $ pure $ pure Nothing

instance KnowledgeGrid STKnowledgeGrid where
	readCell c = STKnowledgeGrid $ \_ r -> Just <$> readArray r c
	writeCell c k = STKnowledgeGrid $ \_ r -> Just <$> writeArray r c k
	getUnknownOrSolution = STKnowledgeGrid $ \d r -> do
		assocs <- getAssocs r
		pure $ Just $ fromFilled d <$> foldl (liftA2 (<>)) (pure mempty) (fmap
			(\(c,k) -> case k of
				Empty -> pure mempty
				Filled -> pure $ S.singleton c
				Unknown -> Left c
			)
			assocs
			)
	isSolved = STKnowledgeGrid $ \_ r ->
		Just <$> foldlMArray' (\a k -> a && (k /= Unknown)) True r
	getSolution = STKnowledgeGrid $ \d r -> do
		assocs <- getAssocs r
		pure $ Just $ fromFilled d <$> foldl (liftA2 (<>)) (pure mempty) (fmap
			(\(c,k) -> case k of
				Empty -> pure mempty
				Filled -> pure $ S.singleton c
				Unknown -> Nothing
			)
			assocs
			)
	getUnknown = STKnowledgeGrid $ \_ r -> do
		assocs <- getAssocs r
		pure $ Just $ listToMaybe $ fmap fst $ filter ((== Unknown) . snd) assocs

runSTKnowledgeGrid :: STKnowledgeGrid x -> Dimensions -> Maybe x
runSTKnowledgeGrid (STKnowledgeGrid f) d = runST $ do
	r <- newArray (minCoordinate, maxCoordinate d) Unknown
	f d r

