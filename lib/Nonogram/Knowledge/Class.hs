{-# LANGUAGE FlexibleContexts #-}

module Nonogram.Knowledge.Class (
	MonadError(..),
	KnowledgeGrid(..),
	NKnowledgeGrid(..),
	) where

import Data.Either (isRight)

import Nonogram.Coordinate (Coordinate)
import Nonogram.Knowledge (Knowledge)
import Nonogram.Solution (Solution)

class Monad m => MonadError m where
	fail :: m ()

class MonadError m => KnowledgeGrid m where
	-- Read a cell's knowledge given its coordinates
	readCell :: Coordinate -> m Knowledge

	-- Write a cell's knowledge given its coordinates and new knowledge
	writeCell :: Coordinate -> Knowledge -> m ()

	-- Get either the coordinates of an unknown, or the solved grid
	getUnknownOrSolution :: m (Either Coordinate Solution)

	-- Check if the grid is solved
	isSolved :: m Bool
	isSolved = isRight <$> getUnknownOrSolution

	-- Get the solution to the grid, if it is solved
	getSolution :: m (Maybe Solution)
	getSolution = either (pure Nothing) Just <$> getUnknownOrSolution

	-- Get the coordinates of an unknown in the grid, if one exists
	getUnknown :: m (Maybe Coordinate)
	getUnknown = either Just (pure Nothing) <$> getUnknownOrSolution

	{-# MINIMAL readCell, writeCell, getUnknownOrSolution #-}

class KnowledgeGrid m => NKnowledgeGrid m where
	-- Nondeterministically pick all non-unknown options for a cell
	tryAll :: Coordinate -> m ()
