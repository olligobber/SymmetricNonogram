module Nonogram.Solution (
	Solution(dimensions),
	fromFilled,
	saveSolution,
	loadSolution,
	getCell,
	) where

import Control.Monad (guard)
import Data.Array (Array)
import qualified Data.Array as A
import Data.Array.ST (runSTArray, newArray, writeArray)
import Data.List (intercalate)
import Data.List.Split (splitOn)

import Nonogram.Coordinate
	( Dimensions(..), Coordinate(Coordinate), Width(..), Height(..)
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

loadSolution :: String -> Maybe Solution
loadSolution s = do
	let
		rowStrings = splitOn "2" s
		h = Height $ length rowStrings
		w = Width $
			if h == Height 0 then
				0
			else
				length $ head rowStrings
		d = Dimensions w h
	guard $ all ((== w) . Width . length) rowStrings
	assocs <- sequence $ do
		(y, row) <- zip (allY h) rowStrings
		(x, char) <- zip (allX w) row
		pure $ (,) (Coordinate x y) <$>
			if char == '0' then
				Just False
			else if char == '1' then
				Just True
			else
				Nothing
	pure $ Solution d $ A.array (minCoordinate, maxCoordinate d) assocs

getCell :: Solution -> Coordinate -> Bool
getCell s c = grid s A.! c