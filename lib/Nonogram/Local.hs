module Nonogram.Local (
	localProgress,
	) where

import Control.Monad.Extra (whileM)

import Nonogram.Coordinate (Coordinate(Coordinate), allX, allY, height, width)
import Nonogram.Hints (Hints)
import qualified Nonogram.Hints as H
import Nonogram.Knowledge (Knowledge(..))
import Nonogram.Knowledge.Class (raise, KnowledgeGrid, readCell, writeCell)

-- Determine all possible solutions to a line given a hint and its knowledge
possibleLines :: [Int] -> [Knowledge] -> [[Bool]]
possibleLines [] [] = [[]]
possibleLines _ [] = []
possibleLines [] (Filled:_) = []
possibleLines [] (_:ks) = (False:) <$> possibleLines [] ks
possibleLines (h:hs) (k:ks) = skip <> fill where
	skip = case k of
		Filled -> []
		_ -> (False:) <$> possibleLines (h:hs) ks
	fill = go h (k:ks)
	go :: Int -> [Knowledge] -> [[Bool]]
	go 0 [] | hs == [] = [[]]
	go 0 (Filled:_) = []
	go 0 (_:kss) = (False:) <$> possibleLines hs kss
	go _ [] = []
	go _ (Empty:_) = []
	go n (_:kss) = (True:) <$> go (n-1) kss

-- Combine all possible solutions into new knowledge, returns Nothing if there are no possible solutions
mergeLines :: [[Bool]] -> Maybe [Knowledge]
mergeLines [] = Nothing
mergeLines bs = Just $ foldl1 (zipWith eitherK) $ toKnowledge <$> bs where
	eitherK _ Unknown = Unknown
	eitherK Unknown _ = Unknown
	eitherK k l
		| k == l = k
		| otherwise = Unknown
	toKnowledge :: [Bool] -> [Knowledge]
	toKnowledge = fmap $ \b -> if b then Filled else Empty

-- Make progress to a single line given its hint, coordinates
-- Returns true if progress was made
progressLine :: KnowledgeGrid m => [Int] -> [Coordinate] -> m Bool
progressLine hint locations = do
	original <- traverse readCell locations
	new <- case mergeLines $ possibleLines hint original of
		Just x -> pure x
		Nothing -> raise
	sequence_ $ zipWith writeCell locations new
	pure $ original /= new

-- Make progress to the entire grid using local logic until no more can be made
localProgress :: forall m. KnowledgeGrid m => Hints -> m ()
localProgress hints =
	let
		dimensions = H.dimensions hints
		colPositions = do
			x <- allX $ width dimensions
			pure $ Coordinate x <$> allY (height dimensions)
		rowPositions = do
			y <- allY $ height dimensions
			pure $ flip Coordinate y <$> allX (width dimensions)
		rowQueries, colQueries :: [m Bool]
		rowQueries = zipWith progressLine (H.rowHints hints) rowPositions
		colQueries = zipWith progressLine (H.colHints hints) colPositions
		allQueries = sequence $ rowQueries <> colQueries
		anyProgress = or <$> allQueries
	in
		whileM anyProgress