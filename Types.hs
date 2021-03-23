module Types (Board, Dimension, Cell, StartingValues, Position, PossibleValue, PossibleValues, Group, Groups) where

type Board = [[Int]]
type Dimension = Int
type Cell = (Position, PossibleValues)
type StartingValues = [Cell]

type Position = (Int, Int)

type PossibleValue = Int
type PossibleValues = [PossibleValue]

type Group = [Position]
type Groups = [Group]
