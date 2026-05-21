import Nonogram.Hints (Hints, loadHints, dimensions)
import Nonogram.Knowledge.Class (NKnowledgeGrid, getUnknown, tryAll)
import Nonogram.Knowledge.GridNST (runGridNST)
import Nonogram.Local (localProgress)

parseHints :: String -> Hints
parseHints s = case loadHints s of
	Just h -> h
	Nothing -> error $ "Could not parse hints: " <> s

numSolutions :: Hints -> Int
numSolutions hints = length $ runGridNST solve dims where
	dims = dimensions hints
	solve :: NKnowledgeGrid m => m ()
	solve = do
		localProgress hints
		unsolved <- getUnknown
		case unsolved of
			Just cell -> tryAll cell *> solve
			Nothing -> pure ()

main :: IO ()
main = interact $
	unlines .
	filter (
		(<= 1) .
		numSolutions .
		parseHints
	) .
	lines