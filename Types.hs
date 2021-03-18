module Types (Position) where

type Position = (Int, Int)
type PossibleValues = [Int]
type GroupId = Int
type Group = (GroupId,[(Int, Int)])

type Cell = (Position, PossibleValues, Group)
