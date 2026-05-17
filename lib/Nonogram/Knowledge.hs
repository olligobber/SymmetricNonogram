module Nonogram.Knowledge (
	Knowledge(..)
	) where

data Knowledge = Empty | Filled | Unknown
	deriving (Eq, Ord, Show)

