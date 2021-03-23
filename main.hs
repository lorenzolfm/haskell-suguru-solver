module Main (main) where

import Functions

main = do

{-
    --Teste 5x5

    -- Informe a dimensão do tabuleiro do jogo.
    -- 5x5, 6x6, ..., n x n.
    let n = 5

    -- Informe os grupos.
    let groups = [[(0,0), (0,1), (0,2), (1,0)], [(0,3), (0,4), (1,4), (2,4), (3,4)], [(1,1), (1,2), (2,0), (2,1), (3,0)], [(1,3), (2,2), (2,3), (3,1), (3,2)], [(3,3), (4,0), (4,1), (4,2), (4,3)], [(4,4)]]

    -- 0,0 0 1
    let pos_0 = (0,0)
    let val_0 = [1]
    let startVal_0 = (pos_0, val_0)

    -- 0,3 1 5
    let pos_1 = (0,3)
    let val_1 = [5]
    let gId_1 = 1
    let startVal_1 = (pos_1, val_1)

    -- 2,0 2 1
    let pos_2 = (2,0)
    let val_2 = [1]
    let startVal_2 = (pos_2, val_2)

    -- 2,2 3 2
    let pos_3 = (2,2)
    let val_3 = [2]
    let startVal_3 = (pos_3, val_3)

    -- 2,4 1 4
    let pos_4 = (2,4)
    let val_4 = [4]
    let startVal_4 = (pos_4, val_4)

    -- 4,1 4 3
    let pos_5 = (4,1)
    let val_5 = [3]
    let startVal_5 = (pos_5, val_5)
    let startValues = [startVal_0, startVal_1, startVal_2, startVal_3, startVal_4, startVal_5]
-}

    let n = 6
    let groups = [[(0,0),(0,1),(0,2),(1,0)],[(0,3),(0,4),(0,5),(1,2),(1,3)],[(1,1),(2,0),(2,1),(2,2),(3,1)],[(1,4),(2,4)],[(1,5),(2,5),(3,5),(4,4),(4,5)],[(2,3),(3,2),(3,3),(3,4),(4,3)],[(3,0),(4,0),(4,1),(4,2),(5,0)],[(5,1),(5,2),(5,3),(5,4),(5,5)]]

    let pos_0 = (0,0)
    let val_0 = [4]
    let startVal_0 = (pos_0, val_0)

    let pos_1 = (0,4)
    let val_1 = [5]
    let startVal_1 = (pos_1, val_1)

    let pos_2 = (2,2)
    let val_2 = [4]
    let startVal_2 = (pos_2, val_2)

    let pos_3 = (3,3)
    let val_3 = [2]
    let startVal_3 = (pos_3, val_3)

    let pos_4 = (3,4)
    let val_4 = [3]
    let startVal_4 = (pos_4, val_4)

    let pos_5 = (4,4)
    let val_5 = [5]
    let startVal_5 = (pos_5, val_5)

    let pos_6 = (5,2)
    let val_6 = [1]
    let startVal_6 = (pos_6, val_6)

    let startValues = [startVal_0, startVal_1, startVal_2, startVal_3, startVal_4, startVal_5, startVal_6]

    let board = createBoard groups startValues n
    let controlList = [0 .. ((n*n)-1)]
    let solved = suguruSolver board groups controlList 0 n
    print (solved)
