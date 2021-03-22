import Debug.Trace

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
removeValueFromPossibleValues list val = [x | x <- list, x /= val]

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

removeAndSet :: [[Int]] -> Position -> Int -> Int -> [[Int]]
removeAndSet board position value dim = do
    let i = (dim * (fst position)) + (snd position)
    let newList = removeValueFromPossibleValues (board !! i) value
    setCorrectValue board position newList dim

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
    let n = getGroupSizeOfPosition groups pos 0
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
    Retorna a Id grupo ao qual a posição pertence

    Param: [[Position]] -> Lista de grupos
    Param: Position -> Posição que deseja-se descobrir o Id de seu Grupo.
    Param: Int -> Id do grupo, usado para recursão.
    Return: Int -> Tamanho do grupo
-}
getGroupIdOfPosition :: [[Position]] -> Position -> Int -> Int
getGroupIdOfPosition groups pos groupID = do
    if (isPositionInGroup (groups !! groupID) pos 0) then
          groupID
        else (getGroupIdOfPosition groups pos (groupID + 1))

{-|
    Retorna o tamanho do grupo ao qual a posição pertence

    Param: [[Position]] -> Lista de grupos
    Param: Position -> Posição que deseja-se descobrir o Id de seu Grupo.
    Param: Int -> Id do grupo, usado para recursão.
    Return: Int -> Id do grupo.
-}
getGroupSizeOfPosition :: [[Position]] -> Position -> Int -> Int
getGroupSizeOfPosition groups pos groupID = do
    if (isPositionInGroup (groups !! groupID) pos 0) then
         length (groups !! groupID)
        else (getGroupSizeOfPosition groups pos (groupID + 1))
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


{-|
    Retorna True se o valor da posição está definido,
    caso contrário retorna False

    Param: [Int] -> Lista de possíveis valores
    Return: Bool -> True se tamanho = 1, False caso contrário
-}
isValueSet :: [Int] -> Bool
isValueSet possibleValues = do
    if (length possibleValues == 1) then
        True
    else
        False

{-|
   Remove um valor passado como argumento de todas as células pertencentes a uma região

  Param: [[Int]] -> O tabuleiro
  Param: Int -> O Id do Grupo
  Param: Int -> O valor a ser removido
-}
removeValueFromAllCellsWithinGroup :: [[Int]] -> Int -> Int -> [[Int]]
removeValueFromAllCellsWithinGroup board groupId value = board

{-|
   Atualiza os possíveis valores de todas as células do tabuleiro
   Exclui dos possiveis valores de uma célular os valores que já estão definidos em uma região.

   Param:
   Param:
   Param:
-}
updatePossibleValuesBySetValuesInGroup :: [[Int]] -> [Position] -> Position -> Int -> Int -> Int -> [[Int]]
updatePossibleValuesBySetValuesInGroup board group position removedValue index dim
  | (index == (length group)) = board
  | otherwise = do
      let x = fst (group !! index)
      let y = snd (group !! index)
      let i = (dim * (fst (group !! index))) + (snd (group !! index))
      if (group !! index == position)
         then updatePossibleValuesBySetValuesInGroup board group position removedValue (index + 1) dim
         else do
            let updatedBoard = removeAndSet board (x,y) removedValue dim
            updatePossibleValuesBySetValuesInGroup updatedBoard group position removedValue (index + 1) dim

{-|
   Atualiza os possíveis valores de todas as células do tabuleiro
   Exclui dos possiveis valores de uma célular os valores que já estão definidos em uma região.

-}
updatePossibleValuesBySetAdjecents :: [[Int]] -> Position -> Int -> Position -> Int ->[[Int]]
updatePossibleValuesBySetAdjecents board position removedValue positionVariable dim
    -- Talvez precise alterar
    | (fst positionVariable) > ((fst position) + 1) = board
    | otherwise = do
        let x = fst positionVariable
        let y = snd positionVariable
        if ((x < 0 || x >= dim || y < 0 || y >= dim) && (y == ((snd position) + 1)))
            then updatePossibleValuesBySetAdjecents board position removedValue ((x+1), (y-2)) dim
            else
            if ((x,y) == position || x < 0 || x >= dim || y < 0 || y >= dim)
                then updatePossibleValuesBySetAdjecents board position removedValue (x, (y+1)) dim
                else do
                    let updatedBoard = removeAndSet board (x,y) removedValue dim
                    if (y == ((snd position) + 1))
                        then updatePossibleValuesBySetAdjecents updatedBoard position removedValue ((x+1), (y-2)) dim
                        else updatePossibleValuesBySetAdjecents updatedBoard position removedValue (x, (y+1)) dim


{-|
    P/ todas células de uma região, se ela possui um possível valor
    que não é compartilhado com mais nenhuma célula da região,
    esse é o valor que a célula deve possuir.
-}
comparePossibleValuesWithinGroup :: [[Int]] -> [Position] -> Position -> Int -> Int -> [[Int]]
comparePossibleValuesWithinGroup board group position index dim
    | (index >= (length (board !! ((dim * (fst (position))) + (snd (position)))))) = board
    | otherwise = do
      let x = fst (position)
      let y = snd (position)
      let i = (dim * (fst (position))) + (snd (position))
      let possibleValues = board !! i
      let comparedValue = possibleValues !! index

      if (compareValue board group position comparedValue 0 dim)
      then do
          let newBoard = setCorrectValue board position [comparedValue] dim
          let otherBoard = updatePossibleValuesBySetValuesInGroup newBoard group position comparedValue 0 dim
          updatePossibleValuesBySetAdjecents otherBoard position comparedValue ((x-1), (y-1)) dim
      else
        comparePossibleValuesWithinGroup board group position (index + 1) dim

compareValue :: [[Int]] -> [Position] -> Position -> Int ->  Int -> Int -> Bool
compareValue board group pos value index dim
    | (index >= (length group)) = True
    | otherwise = do
        let x = fst (group !! index)
        let y = snd (group !! index)
        let i = (dim * (fst (group !! index))) + (snd (group !! index))
        let possibleValues = board !! i
        if (pos == (x,y)) then compareValue board group pos value (index + 1) dim
            else
                if (any (value==) possibleValues) then
                    False
                else
                    compareValue board group pos value (index + 1) dim

mainLoop :: [[Int]] -> [[Position]] -> [Int] -> Int -> Int -> [[Int]]
mainLoop board _ [] _ _ = board
mainLoop board groups list index dim = do
    let indexOfBoard = list !! index
    let possibleValues = board !! indexOfBoard
    let x = indexOfBoard `div` dim
    let y = indexOfBoard `mod` dim
    let pos = (x, y)
    let groupId = getGroupIdOfPosition groups pos 0

    if (isValueSet possibleValues) then do
       let board_0 = updatePossibleValuesBySetValuesInGroup board (groups !! groupId) pos (possibleValues !! 0) 0 dim
       let board_1 = updatePossibleValuesBySetAdjecents board_0 pos (possibleValues !! 0) ((x-1), (y-1)) dim
       let newList = removeValueFromPossibleValues list indexOfBoard
       if ((index + 1) >= (length newList)) then do
            mainLoop board_1 groups newList 0 dim
       else do
           mainLoop board_1 groups newList (index + 1) dim
    else do
        let board_a = comparePossibleValuesWithinGroup board (groups !! groupId) pos 0 dim
        if ((index + 1) >= (length list)) then mainLoop board_a groups list 0 dim
        else mainLoop board_a groups list (index + 1) dim

main = do

{-
    -- Teste 5x5
    let n = 5
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

    let board = createBoard groups startValues n
    let list = [0 .. ((n*n)-1)]

    -- Remover das células os valores já definidos na região a qual ela pertence
    --print board
    --print ""
    let solved = mainLoop board groups list 0 n
    print (solved)
    --
-}

    --Teste 6x6
    let n = 6
    let groups =[[(0,0),(0,1),(0,2),(1,0)],[(0,3),(0,4),(0,5),(1,2),(1,3)],[(1,1),(2,0),(2,1),(2,2),(3,1)],[(1,4),(2,4)],[(1,5),(2,5),(3,5),(4,4),(4,5)],[(2,3),(3,2),(3,3),(3,4),(4,3)],[(3,0),(4,0),(4,1),(4,2),(5,0)],[(5,1),(5,2),(5,3),(5,4),(5,5)]]

    -- Posições Iniciais
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
    let list = [0 .. ((n*n)-1)]
    print board
    print ""
    let solved = mainLoop board groups list 0 n
    print (solved)


