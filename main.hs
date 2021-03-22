import Debug.Trace

type Board = [[Int]]
type Dimension = Int
type PossibleValue = Int
type Position = (Int, Int)
type PossibleValues = [PossibleValue]
type Group = [Position]
type Groups = [Group]
type Cell = (Position, PossibleValues)
type StartingValues = [Cell]


{-|
    Remove o inteiro passado como argumento da lista de possíveis valores.

    Param: PossibleValues -> Uma lista de inteiros, que contém o valor a ser removido
    Param: PossibleValue -> O valor (inteiro) a ser removido da lista

    Return: PossibleValues -> A lista com o valor removido
-}
removeValueFromPossibleValues :: PossibleValues -> PossibleValue -> PossibleValues
removeValueFromPossibleValues possibleValues value = [possibleValue | possibleValue <- possibleValues, possibleValue /= value]


{-|
    Remove um possível valor de uma lista de possíveis valores
    e retorna o tabuleiro atualizado

    Param: Board -> O tabuleiro, uma lista de lista de inteiros.
    Param: Position -> A posição da lista de possíveis valores que será atualizada
    Param: PossibleValue -> Valor a ser removido da lista de possíveis valores
    Param: Dimension -> A dimensão do tabuleiro (Matrix NxN)

    Return: Board -> Tabuleiro com o valor correto inserido.
-}
removeAPossibleValue :: Board -> Position -> PossibleValue -> Dimension -> Board
removeAPossibleValue board position value dimension = do
    let index = (dimension * (fst position)) + (snd position)
    let updatedPossibleValues = removeValueFromPossibleValues (board !! index) value

    setPossibleValues board position updatedPossibleValues dimension


{-|
    Insere no tabuleiro, na posição passada como argumento,
    uma lista que contém um único elemento, sendo esse o elemento
    correto que ocupa a posição no tabuleiro.

    Param: Board -> O tabuleiro, uma lista de lista de inteiros.
    Param: Position -> A posição a ser inserido o valor correto.
    Param: PossibleValue -> O valor correto da posição.
    Param: Dimension -> A dimensão do tabuleiro (Matrix NxN)

    Return: Board -> Tabuleiro com o valor correto inserido.
-}
setCorrectValue :: Board -> Position -> PossibleValue -> Dimension -> Board
setCorrectValue board position correctValue dimension = do
    let tIndex = (dimension * (fst position) + (snd position))
    let dIndex = tIndex + 1

    (take tIndex board) ++ [[correctValue]] ++ (drop dIndex board)


{-|
    Insere no tabuleiro, na posição passada como argumento,
    uma lista de possíveis valores.

    Param: Board -> O tabuleiro, uma lista de lista de inteiros.
    Param: Position -> A posição a ser inserido o valor correto.
    Param: PossibleValues -> Uma lista de possíveis valores
    Param: Dimension -> A dimensão do tabuleiro (Matrix NxN)

    Return: Board -> Tabuleiro com o valor correto inserido.
-}
setPossibleValues :: Board -> Position -> PossibleValues -> Dimension -> Board
setPossibleValues board position possibleValues dimension = do
    let tIndex = dimension * (fst position) + (snd position)
    let dIndex = tIndex + 1

    (take tIndex board) ++ [possibleValues] ++ (drop dIndex board)


{-|
    Função de inicialização do tabuleiro.
    Para cada valor inicial já pré-preenchido, chama setCorrectValue.

    Param: Board -> O tabuleiro, uma lista de lista de inteiros.
    Param: StartingValues -> Lista de células. Os valores inicias que o tabuleiro contém.
    Param: Dimension -> Dimensão do tabuleiro (matriz NxN)

    Return: Board -> Tabuleiro inicializado.
-}
setAllStartingValues :: Board -> StartingValues -> Dimension -> Board
setAllStartingValues board [] _ = board
setAllStartingValues board startingValues dimension = do
    let position = fst (startingValues !! 0)
    let possibleValues = snd (startingValues !! 0)
    let updatedBoard = setPossibleValues board position possibleValues dimension
    let updatedStartingValues = drop 1 startingValues

    setAllStartingValues updatedBoard updatedStartingValues dimension


{-|
    Retorna a Id grupo ao qual a posição pertence.

    Param: Groups -> Lista de grupos
    Param: Position -> Posição que deseja-se descobrir o Id de seu Grupo.
    Param: Int -> Id do grupo, usado para recursão.

    Return: Int -> Id do grupo.
-}
getGroupIdOfPosition :: Groups -> Position -> Int -> Int
getGroupIdOfPosition groups pos groupID = do
    if (isPositionInGroup (groups !! groupID) pos 0) then
          groupID
        else (getGroupIdOfPosition groups pos (groupID + 1))

{-|
    Retorna o tamanho do grupo ao qual a posição pertence

    Param: Groups -> Lista de grupos
    Param: Position -> Posição que deseja-se descobrir o Id de seu Grupo.
    Param: Int -> Id do grupo, usado para recursão.

    Return: Int -> Número de posições pertencentes ao (tamanho do grupo)
-}
getGroupSizeOfPosition :: Groups -> Position -> Int -> Int
getGroupSizeOfPosition groups pos groupID = do
    if (isPositionInGroup (groups !! groupID) pos 0) then
         length (groups !! groupID)
        else (getGroupSizeOfPosition groups pos (groupID + 1))


{-|
    Verifica se uma posição pertence a um grupo.

    Param: Group -> Grupo
    Param: Position -> Posição que deseja-se descobri o Id de seu Grupo.
    Param: Int -> Id do grupo da posição, usado para recursão.
    Return: Bool -> Valor booleano que indica se a posição pertence ao grupo.
-}
isPositionInGroup :: Group -> Position -> Int -> Bool
isPositionInGroup group pos posID = do
    if (posID == (length group)) then
        False
        else
            if ((group !! posID) == pos) then
                True
                else (isPositionInGroup group pos (posID + 1))


{-|
    Inicializa o tabuleiro, cada posição recebe a lista possíveis valores
    de acordo com o tamanho da região ao qual a posição pertence pertence.

    Se a região possui 4 posições, então cada posição pertencente a essa região recebe:
    [1,2,3,4]

    Param: Board -> O tabuleiro, uma lista de lista de inteiros.
    Param: Groups -> Uma lista de lista de posições. Cada lista de posições representa um grupo.
    Param: Int -> Índice usado p/ controlar a recursividade.
    Param: Dimension -> Dimensão do tabuleiro (matriz NxN)

    Return: Board -> Tabuleiro inicializado.
-}
initBoard :: Board -> Groups -> Int -> Dimension -> Board
initBoard board groups index dimension   | index == (dimension * dimension) = board
                                         | otherwise = do
    let x = index `div` dimension
    let y = index `mod` dimension
    let pos = (x, y)

    let n = getGroupSizeOfPosition groups pos 0

    let values = [1..n]
    let updatedBoard = setPossibleValues board pos values dimension

    initBoard updatedBoard groups (index + 1) dimension


{-|
    Cria um tabuleiro, com os requisitos corretos e com os valores iniciais

    Param: Groups -> Grupos (lista de lista de posições)
    Param: StartingValues -> Lista de células, os valores iniciais.
    Param: Dimension -> Dimensão do tabuleiro.

    Return: Board -> Tabuleiro criado
-}
createBoard :: Groups -> StartingValues -> Dimension -> Board
createBoard groups startValues dim = do
    let board = initBoard ([[0] | x <- [1..(dim*dim)]]) groups 0 dim
    setAllStartingValues board startValues dim


{-|
    Retorna True se o valor da posição está definido,
    caso contrário retorna False

    Param: PossibleValues -> Lista de possíveis valores

    Return: Bool -> True se tamanho = 1, False caso contrário
-}
isValueSet :: PossibleValues -> Bool
isValueSet possibleValues = do
    if (length possibleValues == 1) then
        True
    else
        False


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
            let updatedBoard = removeAPossibleValue board (x,y) removedValue dim
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
                    let updatedBoard = removeAPossibleValue board (x,y) removedValue dim
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
          let newBoard = setCorrectValue board position comparedValue dim
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
    --let n = 5
    --let groups = [[(0,0), (0,1), (0,2), (1,0)], [(0,3), (0,4), (1,4), (2,4), (3,4)], [(1,1), (1,2), (2,0), (2,1), (3,0)], [(1,3), (2,2), (2,3), (3,1), (3,2)], [(3,3), (4,0), (4,1), (4,2), (4,3)], [(4,4)]]

    -- 0,0 0 1
    --let pos_0 = (0,0)
    --let val_0 = [1]
    --let gId_0 = 0
    --let startVal_0 = (pos_0, val_0)

    -- 0,3 1 5
    --let pos_1 = (0,3)
    --let val_1 = [5]
    --let gId_1 = 1
    --let startVal_1 = (pos_1, val_1)

    -- 2,0 2 1
    --let pos_2 = (2,0)
    --let val_2 = [1]
    --let gId_2 = 2
    --let startVal_2 = (pos_2, val_2)

    -- 2,2 3 2
    --let pos_3 = (2,2)
    --let val_3 = [2]
    --let gId_3 = 3
    --let startVal_3 = (pos_3, val_3)

    -- 2,4 1 4
    --let pos_4 = (2,4)
    --let val_4 = [4]
    --let gId_4 = 1
    --let startVal_4 = (pos_4, val_4)

    -- 4,1 4 3
    --let pos_5 = (4,1)
    --let val_5 = [3]
    --let gId_5 = 4
    --let startVal_5 = (pos_5, val_5)

    --let startValues = [startVal_0, startVal_1, startVal_2, startVal_3, startVal_4, startVal_5]

    --let board = createBoard groups startValues n
    --let list = [0 .. ((n*n)-1)]
    --print list

    -- Remover das células os valores já definidos na região a qual ela pertence
    --print board
    --print ""
    --let solved = mainLoop board groups list 0 n
    --print (solved)
    --

    --Teste 6x6
    let n = 6
    let groups =[[(0,0),(0,1),(0,2),(1,0)],[(0,3),(0,4),(0,5),(1,2),(1,3)],[(1,1),(2,0),(2,1),(2,2),(3,1)],[(1,4),(2,4)],[(1,5),(2,5),(3,5),(4,4),(4,5)],[(2,3),(3,2),(3,3),(3,4),(4,3)],[(3,0),(4,0),(4,1),(4,2),(5,0)],[(5,1),(5,2),(5,3),(5,4),(5,5)]]

    -- Posições Iniciais
    --let pos_5 = (4,1)
    --let val_5 = [3]
    --let startVal_5 = (pos_5, val_5)
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
    --print board
    --print ""
    let solved = mainLoop board groups list 0 n
    print (solved)
