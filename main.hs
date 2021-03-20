type Position = (Int, Int)
type PossibleValues = [Int]
type GroupId = Int
type Group = [(Int, Int, GroupId)]
type Cell = (Position, PossibleValues)

{-|
    Remove o inteiro passado como argumento da lista

    Param: [Int] -> Uma lista de inteiros, que contém o valor a ser removido
    Param: Int -> O inteiro a ser removido da lista
    Return: [Int] -> A lista com o valor removido
-}
removeValueFromPossibleValues :: [Int] -> Int -> [Int]
removeValueFromPossibleValues list val= [x | x <- list, x /= val]

{-|
    Insere no tabuleiro, na posição passada como argumento,
    uma lista que contém um único elemento, sendo esse o elemento
    correto que ocupa a posição no tabuleiro.

    Param: [[Int]] -> O tabuleiro, uma lista de lista de inteiros.
    Param: Position -> A posição a ser inserido o valor correto.
    Param: [Int] -> Uma lista que contém apenas o valor a ser inserido na lista.
    Param: Int -> A dimensão do tabuleiro (Matrix NxN)
    Return: [[Int]] -> Tabuleiro com o valor correto inserido.
-}
setCorrectValue :: [[Int]] -> Position -> [Int] -> Int -> [[Int]]
setCorrectValue matrix position correctValue dimension = (take (dimension * (fst position) + (snd position)) matrix) ++ [correctValue] ++ (drop ((dimension * (fst position) + (snd position))+1) matrix)

{-|
    Função de inicialização do tabuleiro.
    Para cada valor inicial já pré-preenchido, chama setCorrectValue.

    Param: [[Int]] -> O tabuleiro, uma lista de lista de inteiros.
    Param: Cell -> Lista de células. Os valores inicias que o tabuleiro contém.
    Param: Int -> Dimensão do tabuleiro (matriz NxN)
    Return: [[Int]] -> Tabuleiro inicializado.
-}
setAllStartingValues :: [[Int]] -> [Cell] -> Int -> [[Int]]
setAllStartingValues matrix [] _ = matrix
setAllStartingValues matrix startValues dim = do
    let new_matrix = setCorrectValue matrix (fst (startValues !! 0)) (snd (startValues !!0)) dim
    let new_values = drop 1 startValues
    setAllStartingValues new_matrix new_values dim

{-|
    Inicializa o tabuleiro, cada posição recebe a lista possíveis valores
    de acordo com o tamanho da região ao qual a posição pertence pertence.

    Se a região possui 4 posições, então cada posição pertencente a essa região recebe:
    [1,2,3,4]

    Param: [[Int]] -> O tabuleiro, uma lista de lista de inteiros.
    Param: [[Position]] -> Uma lista de lista de posições. Cada lista de posições representa um grupo.
    Param: Int -> Índice usado p/ controlar a recursividade.
    Return: [[Int]] -> Tabuleiro inicializado.
-}
initBoard :: [[Int]] -> [[Position]] -> Int -> Int -> [[Int]]
initBoard matrix groups index dim   | index == (dim * dim) = matrix
                                    | otherwise = do
    let x = index `div` dim
    let y = index `mod` dim
    let pos = (x, y)
    let n = getGroupIdOfPosition groups pos 0
    let values = [1..n]
    -- Função setCorrectValue é usada p/ inicializar o tabuleiro, antes de inserir os valores pré-preenchidos
    let new_matrix = setCorrectValue matrix pos values dim

    initBoard new_matrix groups (index + 1) dim

{-|
    Retorna o tamanho do grupo, ou seja, quantos posições ele possui

    Param: [Position] -> Lista de posições pertencentes ao grupo.
    Return: Int -> Tamanho do grupo
-}
getGroupSize :: [Position] -> Int
getGroupSize group = length group

{-|
    Retorna o ID do grupo ao qual a posição pertence

    TODO: Fazer adaptação para pegar `groupID` ou retornar o grupo em si

    Param: [[Position]] -> Lista de grupos
    Param: Position -> Posição que deseja-se descobrir o Id de seu Grupo.
    Param: Int -> Id do grupo, usado para recursão.
    Return: Int -> Id do grupo.
-}
getGroupIdOfPosition :: [[Position]] -> Position -> Int -> Int
getGroupIdOfPosition groups pos groupID = do
    if (isPositionInGroup (groups !! groupID) pos 0) then
         length (groups !! groupID)
        else (getGroupIdOfPosition groups pos (groupID + 1))

{-|
    Verifica se uma posição pertence a um grupo.

    Param: [Position] -> Grupo
    Param: Position -> Posição que deseja-se descobri o Id de seu Grupo.
    Param: Int -> Id do grupo da posição, usado para recursão.
    Return: Bool -> Valor booleano que indica se a posição pertence ao grupo.
-}
isPositionInGroup :: [Position] -> Position -> Int -> Bool
isPositionInGroup group pos posID = do
    if (posID == (length group)) then
        False
        else
            if ((group !! posID) == pos) then
                True
                else (isPositionInGroup group pos (posID + 1))


{-|
    Cria um tabuleiro, com os requisitos corretos e com os valores iniciais

    Param: [[Position]] -> Tabuleiro
    Param: [Cell] -> Lista de células, os valores iniciais.
    Param: Int -> Dimensão do tabuleiro.
    Return: [[Int]] -> Tabuleiro criado
-}
createBoard :: [[Position]] -> [Cell] -> Int -> [[Int]]
createBoard groups startValues dim = do
    let board = initBoard ([[0] | x <- [1..(dim*dim)]]) groups 0 dim
    setAllStartingValues board startValues dim

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

    let new = setAllStartingValues new_board startValues n
    print new
    print (createBoard groups startValues n)
