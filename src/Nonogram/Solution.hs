module Nonogram.Solution (
	fromFilled,
	saveSolution,
	) where

import Data.Array (Array)
import qualified Data.Array as A
import Data.Array.ST (runSTArray, newArray, writeArray)
import Data.List (intercalate)

import Nonogram.Coordinate
	( Dimensions(width, height), Coordinate(Coordinate)
	, minCoordinate, maxCoordinate, allX, allY
	)

data Solution = Solution
	{ dimensions :: Dimensions
	, grid :: Array Coordinate Bool
	} deriving (Show)

fromFilled :: Foldable f => Dimensions -> f Coordinate -> Solution
fromFilled d filled = Solution d $ runSTArray $ do
	a <- newArray (minCoordinate, maxCoordinate d) False
	mapM_ (\c -> writeArray a c True) filled
	pure a

saveSolution :: Solution -> String
saveSolution s = intercalate "2" $ renderRow <$> allY (height $ dimensions s) where
	renderRow y = renderCell <$> allX (width $ dimensions s) <*> pure y
	renderCell x y = if grid s A.! Coordinate x y then '1' else '0'