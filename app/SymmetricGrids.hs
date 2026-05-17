import Data.Foldable (fold)
import Data.Set (Set)
import qualified Data.Set as S

import Nonogram.Coordinate
	( Width(Width), Height(Height), Dimensions(Dimensions), Coordinate
	, allCoordinates, symmetries
	)

import Nonogram.Solution (fromFilled, saveSolution)

newtype SymmetryClass = SymmetryClass { fromClass :: Set Coordinate }
	deriving (Eq, Ord)

instance Semigroup SymmetryClass where
	SymmetryClass s <> SymmetryClass t = SymmetryClass $ s <> t

instance Monoid SymmetryClass where
	mempty = SymmetryClass mempty

main :: IO ()
main = do
	width <- Width <$> readLn
	height <- Height <$> readLn
	let
		dimensions :: Dimensions
		dimensions = Dimensions width height

		symmetrySets :: Set SymmetryClass
		symmetrySets = S.fromList $ SymmetryClass . symmetries dimensions <$> allCoordinates dimensions

		filleds :: Set SymmetryClass
		filleds = S.map fold $ S.powerSet symmetrySets

	mapM_ (putStrLn . saveSolution . fromFilled dimensions . fromClass) filleds