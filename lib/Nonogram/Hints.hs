module Nonogram.Hints (
	Hints,
	fromSolution,
	saveHints,
	) where

import Data.List (group, intercalate)

import Nonogram.Coordinate
	( Dimensions(width, height), Coordinate(Coordinate)
	, allX, allY
	)
import Nonogram.Solution (Solution, getCell)
import qualified Nonogram.Solution as S

data Hints = Hints
	{ dimensions :: Dimensions
	, rowHints :: [[Int]]
	, colHints :: [[Int]]
	} deriving (Show)

fromSolution :: Solution -> Hints
fromSolution s = Hints d rHints cHints where
	d = S.dimensions s
	rHints = rHint <$> allY (height d)
	rHint y = fmap length $ filter head $ group $ do
		x <- allX $ width d
		pure $ getCell s $ Coordinate x y
	cHints = cHint <$> allX (width d)
	cHint x = fmap length $ filter head $ group $ do
		y <- allY $ height d
		pure $ getCell s $ Coordinate x y

saveHints :: Hints -> String
saveHints h = saveAll rowHints <> "/" <> saveAll colHints where
	saveAll :: (Hints -> [[Int]]) -> String
	saveAll hints = intercalate ";" $ saveHint <$> hints h

	saveHint :: [Int] -> String
	saveHint hint = intercalate "," $ show <$> hint