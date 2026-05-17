import Data.Maybe (fromJust)

import Nonogram.Solution (loadSolution)
import Nonogram.Hints (saveHints, fromSolution)

main :: IO ()
main =
	interact $
		unlines .
		fmap (
			saveHints .
			fromSolution .
			fromJust .
			loadSolution
		) .
		lines