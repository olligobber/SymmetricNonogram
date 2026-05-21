module Nonogram.Hints (
	Hints(dimensions, rowHints, colHints),
	fromSolution,
	saveHints,
	loadHints,
	) where

import Data.List (group, intercalate)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

import Nonogram.Coordinate
	( Dimensions(..), Coordinate(Coordinate), Width(..), Height(..)
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

loadHints :: String -> Maybe Hints
loadHints s = do
	(rowString, colString) <- case splitOn "/" s of
		[a,b] -> Just (a,b)
		_ -> Nothing
	rHints <- parseHints rowString
	cHints <- parseHints colString
	pure $ Hints
		(Dimensions (Width $ length cHints) (Height $ length rHints))
		rHints
		cHints
	where
		parseHints :: String -> Maybe [[Int]]
		parseHints ss = traverse parseHint $ splitOn ";" ss
		parseHint :: String -> Maybe [Int]
		parseHint [] = Just []
		parseHint ss = traverse readMaybe $ splitOn "," ss