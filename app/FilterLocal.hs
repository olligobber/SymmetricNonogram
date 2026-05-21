import Nonogram.Hints (Hints, loadHints, dimensions)
import Nonogram.Knowledge.Class (KnowledgeGrid, isSolved)
import Nonogram.Knowledge.GridST (runGridST)
import Nonogram.Local (localProgress)

parseHints :: String -> Hints
parseHints s = case loadHints s of
	Just h -> h
	Nothing -> error $ "Could not parse hints: " <> s

isLocal :: Hints -> Bool
isLocal hints = runGridST solve dims == Just True where
	dims = dimensions hints
	solve :: KnowledgeGrid m => m Bool
	solve = localProgress hints *> isSolved

main :: IO ()
main = interact $
	unlines .
	filter (
		not .
		isLocal .
		parseHints
	) .
	lines