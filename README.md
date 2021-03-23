# Suguru Solver

Lorenzo Maturano - 19100535
Gabriel Carneiro - 19103977

![Tabuleiro](imgs/suguru_1.png)

## Objetivo

O objetivo do trabalho é desenvolver um resolvedor do puzzle **Suguru** em **Haskell**.

## Regras do Jogo

* O Suguru é um *puzzle* de tabuleiro, composto por uma **matriz quadrada *nxn* de qualquer tamanho**.
* A matriz é **dividida em grupos** e os **grupos são formados por células**. As células são equivalentes a um **elemento da matriz**
* Um grupo de *N* células deve, **obrigatoriamente, conter os números de 1 a *N***.
* Se uma célula possui um valor *m*, **todas as células adjacentes não podem conter o mesmo valor**.

### Compilação e Execução

* Compile o programa com: `ghc --make main.hs`.
* Execute com: `./main`.

```
ghc --make main.hs && ./main
```

### Solução Proposta

#### Entrada:

A entrada é feita diretamente no código do programa, no arquivo `main.hs`.

![Tabuleiro](imgs/suguru_1.png)

* `n` indica a dimensão do tabuleiro.

```
let n = 5
```

* `groups` é uma lista de grupos, cada grupo armazena tuplas (i,j) que indicam as posições (a<sub>i,j</sub>) que o grupo contém. Para definir os grupos, use uma lista para cada grupo e entre com as tuplas (i, j) referentes posição da célula no tabuleiro. Exemplo:


```
let groups = [[(0,0), (0,1), (0,2), (1,0)], [(0,3), (0,4), (1,4), (2,4), (3,4)], [(1,1), (1,2), (2,0), (2,1), (3,0)], [(1,3), (2,2), (2,3), (3,1), (3,2)], [(3,3), (4,0), (4,1), (4,2), (4,3)], [(4,4)]]
```

* Em seguida, deve-se informar os valores iniciais que o tabuleiro possui. Cada valor inicial é composto por uma tupla (posição, valor). Onde o valor é uma lista com um único elemento. Exemplo:

```
let pos_0 = (0,0)
let val_0 = [1]
let startVal_0 = (pos_0, val_0)
```

* Insira todas as tuplas de valores iniciais em uma lista.

```
let startValues = [startVal_0, startVal_1, startVal_2, startVal_3, startVal_4, startVal_5, startVal_6]
```

#### Saída

A saída do programa é uma lista de listas, cada lista contém apenas um elemento, sendo esse o elemento a solução do quebra-cabeça.

##### Interpretação da Saída

Cada elemento da lista resultante é referente a uma posição (i, j) do tabuleiro, onde os *n* primeiros elementos representam a linha ao topo do tabuleiro, da esquerda para a direita.

* Exemplo:

![Resolvido](img/resolvido.png)

```
[[1],[3],[2],[5],[1],[4],[5],[4],[3],[2],[1],[3],[2],[1],[4],[2],[5],[4],[5],[3],[4],[3],[1],[2],[1]]
```

Interprete como:

```
[1],[3],[2],[5],[1],
[4],[5],[4],[3],[2],
[1],[3],[2],[1],[4],
[2],[5],[4],[5],[3],
[4],[3],[1],[2],[1]
```

#### Algoritmo:

1. O algoritmo começa com a criação do tabuleiro `createBoard`.

2. Após isso, é criado uma lista de controle `controlList` que contém todas as posições a serem resolvidas.

3. Em seguida, é chamada a função `suguruSolver` e passado como argumento o tabuleiro os grupos a lista de controle um índice inicial (0) e a dimensão.

4. `suguruSolver` executa iterativamente enquanto a lista de controle não estiver vazia.

5. Caso a lista não esteja vazia:

a) Se a posição atual possui um valor definido (lista de possíveis valores com apenas um elemento) então atualizamos todas as listas de possíveis valores das demais posições do grupo `updatePossibleValuesBySetValuesInGroup`, removendo o valor definido. Em seguida, atualizamos as posições adjacentes de maneira similar `updatePossibleValuesBySetAdjecents`. Finalmente, removemos a posição da lista de controle e chamamos a função `suguruSolver` recursivamente.

b) Se a posição atual não possui um único valor possível, é checado se sua lista de possíveis valores possui algum valor que é único entre todas as demais posições do grupo `comparePossibleValuesWithinGroup`. Caso positivo o valor que é único é o valor que deve ocupar a posição. Por fim, chamamos a função `suguruSolver` recursivamente.

* Exemplo:

```
grupo = [[1,2,3,4,5], [1,2,3,4], [1,2,3,4], [1,2,3,4], [1,2,3,4]]
```

Nesse caso, a primeira posição do grupo é a única que possui 5 como possível valor, então 5 é o valor daquela posição.

## Organização do Grupo

O grupo usou o git, github e gitflow para implementar o trabalho, sendo assim possível desenvolver de maneira assíncrona diferentes features do programa. Também foram feitas seções de Pair Programming com um dos alunos escrevendo e o outro supervisionando.

Gabriel ficou responsável pelas funções de checagem de corretude. Lorenzo ficou encarregado pelas funções de manipulação dos dados.

A maior dificuldade foi implementar algoritmos iterativos recursivamente. A solução adotada foi segmentar os procedimentos em funções pequenas, facilitando sua iteração e integração.

## Anexos

```
{-|
    Atualiza os possíveis valores de todas as células de um grupo.
    Exclui dos possiveis valores de uma célula os valores que já estão definidos em uma região.

    Param: Board -> O tabuleiro, uma lista de lista de inteiros.
    Param: Group -> Grupo, uma lista de posições.
    Param: Position -> Posição (tupla (x,y)) a posição da célula que vamos excluir.
    Param: PossibleValue -> Possível valor, um inteiro.
    Param: Int -> Index, usado p/ recursão.
    Param: Dimension -> Dimensão do tabuleiro (matriz NxN).

    Return: Board -> O tabuleiro atualizado.
-}
updatePossibleValuesBySetValuesInGroup :: Board -> Group -> Position -> PossibleValue -> Int -> Dimension -> Board
updatePossibleValuesBySetValuesInGroup board group position removedValue index dim
  | (index == (length group)) = board
  | otherwise = do
      let x = fst (group !! index)
      let y = snd (group !! index)
      let i = (dim * x) + y
      if (group !! index == position)
         then updatePossibleValuesBySetValuesInGroup board group position removedValue (index + 1) dim
         else do
            let updatedBoard = removeAPossibleValue board (x,y) removedValue dim
            updatePossibleValuesBySetValuesInGroup updatedBoard group position removedValue (index + 1) dim
```

```
{-|
    Atualiza os possíveis valores das células adjacentes a uma célula que tem seu valor já definido.
    Exclui dos possiveis valores das células adjacentes o valor definido na célula.

    Param: Board -> Tabuleiro do jogo.
    Param: Position -> Uma tupla (x, y), posição "sob análise"
    Param: PossibleValue -> Valor a ser removido.
    Param: Position -> Uma tupla (x, y), posição do adjacente.
    Param: Dimension -> Dimensão do tabuleiro (matriz NxN).

    Return: Board -> Tabuleiro do jogo atualizado.

-}
updatePossibleValuesBySetAdjecents :: Board -> Position -> PossibleValue -> Position -> Dimension -> Board
updatePossibleValuesBySetAdjecents board position removedValue positionVariable dim
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
```

```
{-|
    P/ todas células de uma região, se ela possui um possível valor
    que não é compartilhado com mais nenhuma célula da região,
    esse é o valor que a célula deve possuir.

    Essa função compara os possíveis valores entre as posições que pertencem
    a região. Caso algum valor seja único em alguma das posições da região,
    os outros possíveis valores da lista são excluídos e, em seguida, é chamado
    updatePossibleValuesBySetValuesInGroup e updatePossibleValuesBySetAdjecents


    Param: Board -> Tabuleiro do jogo.
    Param: Group -> Grupo sob análise.
    Param: Position -> Posição de alguma célula do grupo
    Param: Int -> Índice usado para controlar a recursividade.
    Param: Dimension -> Dimensão do tabuleiro (matriz NxN).

    Return: Board -> Tabuleiro do jogo atualizado.
-}
comparePossibleValuesWithinGroup :: Board -> Group -> Position -> Int -> Dimension -> Board
comparePossibleValuesWithinGroup board group position index dim
    | (index >= (length (board !! ((dim * (fst (position))) + (snd (position)))))) = board
    | otherwise = do
      let x = fst (position)
      let y = snd (position)
      let i = (dim * x) + y

      let possibleValues = board !! i
      let comparedValue = possibleValues !! index

      if (compareValue board group position comparedValue 0 dim)
      then do
          let newBoard = setCorrectValue board position comparedValue dim
          let otherBoard = updatePossibleValuesBySetValuesInGroup newBoard group position comparedValue 0 dim
          updatePossibleValuesBySetAdjecents otherBoard position comparedValue ((x-1), (y-1)) dim
      else
        comparePossibleValuesWithinGroup board group position (index + 1) dim
```

```
{-|
   Recebe um tabuleiro de suguru e o resolve, chamando as funções auxiliares declaradas anteriormente.

    Param: Board -> Tabuleiro apenas com os valores iniciais.
    Param: Groups -> Grupos pertencentes ao tabuleiro.
    Param: [Int] -> Lista de controle, que controla quantas posições ainda não tiveram seus valores determinados.
    Param: Int -> Índice usado para percorer o tabuleiro.
    Param: Dimension -> Dimensão do tabuleiro (matriz NxN).

    Return: Board -> Tabuleiro resolvido.
-}
suguruSolver :: Board -> Groups -> [Int] -> Int -> Dimension -> Board
suguruSolver board _ [] _ _ = board
suguruSolver board groups list index dim = do
    let indexOfBoard = list !! index
    let possibleValues = board !! indexOfBoard

    let pos = getPosition indexOfBoard dim
    let x = fst pos
    let y = snd pos

    let groupId = getGroupIdOfPosition groups pos 0

    if (isValueSet possibleValues) then do
       let board_0 = updatePossibleValuesBySetValuesInGroup board (groups !! groupId) pos (possibleValues !! 0) 0 dim
       let board_1 = updatePossibleValuesBySetAdjecents board_0 pos (possibleValues !! 0) ((x-1), (y-1)) dim
       let newList = removeValueFromPossibleValues list indexOfBoard

       if ((index + 1) >= (length newList)) then do
            suguruSolver board_1 groups newList 0 dim
       else do
           suguruSolver board_1 groups newList (index + 1) dim

    else do
        let board_a = comparePossibleValuesWithinGroup board (groups !! groupId) pos 0 dim

        if ((index + 1) >= (length list))
           then suguruSolver board_a groups list 0 dim
        else suguruSolver board_a groups list (index + 1) dim
```
