import Nonogram.Solution (Solution, loadSolution)
import Nonogram.Hints (saveHints, fromSolution)

parseSolution :: String -> Solution
parseSolution s = case loadSolution s of
	Just x -> x
	Nothing -> error $ "Could not parse solution: " <> s

main :: IO ()
main = interact $
	unlines .
	fmap (
		saveHints .
		fromSolution .
		parseSolution
	) .
	lines