type Position = (Int, Int)
type PossibleValues = [Int]
type GroupId = Int
type Group = [(Int, Int, GroupId)]
type Cell = (Position, [Int])

-- Insere no tabuleiro um valor inicial
initVal :: [[Int]] -> Position -> [Int] -> Int -> [[Int]]
initVal matrix pos val dim = (take (dim * (fst pos) + (snd pos)) matrix) ++ [val] ++ (drop ((dim * (fst pos) + (snd pos))+1) matrix)

-- Chama a função `initVal` para cada valor inicial
startVal :: [[Int]] -> [Cell] -> Int -> [[Int]]
startVal matrix [] _ = matrix
startVal matrix startValues dim = do
    let new_matrix = initVal matrix (fst (startValues !! 0)) (snd (startValues !!0)) dim
    let new_values = drop 1 startValues
    startVal new_matrix new_values dim

-- Inicializa o tabuleiro, cada posição recebe a lista de possibilidades correta de acordo com a região ao qual pertence
initBoard :: [[Int]] -> [[Position]] -> Int -> Int -> [[Int]]
initBoard matrix groups index dim   | index == (dim * dim) = matrix
                                    | otherwise = do
    let x = index `div` dim
    let y = index `mod` dim
    let pos = (x, y)
    let n = getPosGroup groups pos 0
    let values = [1..n]
    let new_matrix = initVal matrix pos values dim

    initBoard new_matrix groups (index + 1) dim


-- Retorna o tamanho do grupo, ou seja, quantos posições ele possui
getSizeGroup :: [Position] -> Int
getSizeGroup group = length group

-- Retorna o ID do grupo ao qual a posição pertence
-- Fazer adaptação para pegar `groupID` ou retornar o grupo em si
getPosGroup :: [[Position]] -> Position -> Int -> Int
getPosGroup groups pos groupID = do
    if (checkPosInGroup (groups !! groupID) pos 0) then
         length (groups !! groupID)
        else (getPosGroup groups pos (groupID + 1))

-- Verifica se a posição pertence ao grupo
checkPosInGroup :: [Position] -> Position -> Int -> Bool
checkPosInGroup group pos posID = do
    if (posID == (length group)) then
        False
        else
            if ((group !! posID) == pos) then
                True
                else (checkPosInGroup group pos (posID + 1))

-- Cria um tabuleiro, com os requisitos corretos e com os valores iniciais
createBoard :: [[Position]] -> [Cell] -> Int -> [[Int]]
createBoard groups startValues dim = do
    let board = initBoard ([[0] | x <- [1..(dim*dim)]]) groups 0 dim
    startVal board startValues dim

main = do
    let n = 5
    let groups = [[(0,0), (0,1), (0,2), (1,0)], [(0,3), (0,4), (1,4), (2,4), (3,4)], [(1,1), (1,2), (2,0), (2,1), (3,0)], [(1,3), (2,2), (2,3), (3,1), (3,2)], [(3,3), (4,0), (4,1), (4,2), (4,3)], [(4,4)]]

    -- 0,0 0 1
    let pos_0 = (0,0)
    let val_0 = [1]
    let gId_0 = 0
    let startVal_0 = (pos_0, val_0)

    -- 0,3 1 5
    let pos_1 = (0,3)
    let val_1 = [5]
    let gId_1 = 1
    let startVal_1 = (pos_1, val_1)

    -- 2,0 2 1
    let pos_2 = (2,0)
    let val_2 = [1]
    let gId_2 = 2
    let startVal_2 = (pos_2, val_2)

    -- 2,2 3 2
    let pos_3 = (2,2)
    let val_3 = [2]
    let gId_3 = 3
    let startVal_3 = (pos_3, val_3)

    -- 2,4 1 4
    let pos_4 = (2,4)
    let val_4 = [4]
    let gId_4 = 1
    let startVal_4 = (pos_4, val_4)

    -- 4,1 4 3
    let pos_5 = (4,1)
    let val_5 = [3]
    let gId_5 = 4
    let startVal_5 = (pos_5, val_5)

    let startValues = [startVal_0, startVal_1, startVal_2, startVal_3, startVal_4, startVal_5]
    --print (fst (startValues !! 0))

    let board = [[1..5] | x <- [1..(n*n)]]
    --print board
    let new_board = initBoard board groups 0 n
    --print new_board

    let new = startVal new_board startValues n
    print new
    print (createBoard groups startValues n)
