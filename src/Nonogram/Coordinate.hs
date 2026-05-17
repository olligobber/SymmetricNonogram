module Nonogram.Coordinate (
	Width(Width),
	Height(Height),
	Dimensions(..),
	Horizontal,
	Vertical,
	Coordinate(..),
	minX,
	maxX,
	minY,
	maxY,
	minCoordinate,
	maxCoordinate,
	allX,
	allY,
	allCoordinates,
	symmetries
	) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Ix (Ix)

newtype Width = Width Int
	deriving (Show)

newtype Height = Height Int
	deriving (Show)

data Dimensions = Dimensions { width :: Width, height :: Height }
	deriving (Show)

isSquare :: Dimensions -> Bool
isSquare (Dimensions (Width w) (Height h)) = w == h

newtype Horizontal = Horizontal Int
	deriving (Eq, Ord, Show, Ix)

newtype Vertical = Vertical Int
	deriving (Eq, Ord, Show, Ix)

data Coordinate = Coordinate { x :: Horizontal, y :: Vertical }
	deriving (Eq, Ord, Show, Ix)

minX :: Horizontal
minX = Horizontal 1

maxX :: Width -> Horizontal
maxX (Width w) = Horizontal w

minY :: Vertical
minY = Vertical 1

maxY :: Height -> Vertical
maxY (Height h) = Vertical h

minCoordinate :: Coordinate
minCoordinate = Coordinate minX minY

maxCoordinate :: Dimensions -> Coordinate
maxCoordinate d = Coordinate (maxX $ width d) (maxY $ height d)

allX :: Width -> [Horizontal]
allX (Width w) = Horizontal <$> [1 .. w]

allY :: Height -> [Vertical]
allY (Height h) = Vertical <$> [1 .. h]

allCoordinates :: Dimensions -> [Coordinate]
allCoordinates d = Coordinate <$> allX (width d) <*> allY (height d)

reflectX :: Width -> Horizontal -> Horizontal
reflectX (Width w) (Horizontal h) = Horizontal (w - h + 1)

reflectY :: Height -> Vertical -> Vertical
reflectY (Height h) (Vertical v) = Vertical (h - v + 1)

symmetries :: Dimensions -> Coordinate -> Set Coordinate
symmetries d c
	| isSquare d = S.fromList $ rectSymmetries c <> rectSymmetries (reflectDiag c)
	| otherwise = S.fromList $ rectSymmetries c
	where
		rectSymmetries cc =
			[ cc
			, Coordinate (x cc) (reflectY (height d) (y cc))
			, Coordinate (reflectX (width d) (x cc)) (y cc)
			, Coordinate (reflectX (width d) (x cc)) (reflectY (height d) (y cc))
			]
		reflectDiag cc =
			let
				Horizontal ccx = x cc
				Vertical ccy = y cc
			in
				Coordinate (Horizontal ccy) (Vertical ccx)

