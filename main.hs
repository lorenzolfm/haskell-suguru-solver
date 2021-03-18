type Position = (Int, Int)
type PossibleValues = [Int]
type GroupId = Int
type Group = [(Int, Int, GroupId)]
type Cell = (Position, PossibleValues, GroupId)

--setValue :: [[Int]] -> Position -> Int -> Int -> [Int]
--setValue list pos val dim = (take (dim * (fst pos) + (snd pos)) list) ++ [val] ++ (drop ((dim * (fst pos) + (snd pos))+1) list)

--setValue :: [[Int]] -> Position -> Int -> [[Int]]
--setValue matrix pos val = (take (fst pos) matrix) ++ [(take (snd pos) (matrix!!(fst pos))) ++ [val] ++ (drop ((snd pos)+1) (matrix!!(fst pos)))] ++ (drop ((fst pos)+1) matrix)


--setValue' :: [[Int]] -> Position -> Int -> [[Int]]
--setValue' matrix pos val = (take (fst pos) matrix) ++ [[val]] ++ (drop ((fst pos)+1) matrix)

setValue' :: [[Int]] -> Position -> Int -> Int -> [[Int]]
setValue' matrix pos val dim = (take (dim * (fst pos) + (snd pos)) matrix) ++ [[val]] ++ (drop ((dim * (fst pos) + (snd pos))+1) matrix)

-- Remove um valor da lista
removeValue :: [Int] -> Int -> [Int]
removeValue list val= [x | x <- list, x /= val]

main = do
  let n = 5
  let numGroups = 6
  let numStartingValues = 6

  let groups = [[(0,0), (0,1), (0,2), (1,0)], [(0,3), (0,4), (1,4), (2,4), (3,4)], [(1,1), (1,2), (2,0), (2,1), (3,0)], [(1,3), (2,2), (2,3), (3,1), (3,2)], [(3,3), (4,0), (4,1), (4,2), (4,3)], [(4,4)]]

  -- 0,0 0 1
  let pos_0 = (0,0)
  let val_0 = [1]
  let gId_0 = 0
  let startVal_0 = (pos_0, val_0, gId_0)

  -- 0,3 1 5
  let pos_1 = (0,3)
  let val_1 = [5]
  let gId_1 = 1
  let startVal_1 = (pos_1, val_1, gId_1)

  -- 2,0 2 1
  let pos_2 = (2,0)
  let val_2 = [1]
  let gId_2 = 2
  let startVal_2 = (pos_2, val_2, gId_2)

  -- 2,2 3 2
  let pos_3 = (2,2)
  let val_3 = [2]
  let gId_3 = 3
  let startVal_3 = (pos_3, val_3, gId_3)

  -- 2,4 1 4
  let pos_4 = (2,4)
  let val_4 = [4]
  let gId_4 = 1
  let startVal_4 = (pos_4, val_4, gId_4)

  -- 4,1 4 3
  let pos_5 = (4,1)
  let val_5 = [3]
  let gId_5 = 4
  let startVal_5 = (pos_5, val_5, gId_5)

  let startValues = [startVal_0, startVal_1, startVal_2, startVal_3, startVal_4, startVal_5]

  let board = [ [1..5] | x <- [1..(n*n)]]

  let oneCell = board !! 0
  let newList = removeValue oneCell 3
  print newList

  let newList2 = removeValue newList 2
  print newList2
