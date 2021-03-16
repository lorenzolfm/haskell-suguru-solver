module Main (main) where

import FileIO
import Control.Monad
import System.IO


main = do
  -- FileIO
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let list = lines contents

  -- Dimensão da Matriz
  let n = convertStrInt (dimension list)
  -- Numero de Regiões
  let numOfRegions = convertStrInt (dimension (slice list 1 2))
  -- Numero de pontos iniciais
  let numOfStartingNumbers = convertStrInt (dimension (slice list 2 3))

  -- Regiões
  let regions = slice list 3 4

  -- Números já preenchidos e suas posições
  let startingNumbersAndPositions = slice list (4) (4 + numOfStartingNumbers)

  --print n
  --print numOfRegions
  --print numOfStartingNumbers
  --print regions
  --print startingNumbersAndPositions
  -- Fim IO

  -- Estruturação de dados

  -- Matriz do jogo
  let puzzle = matrix n n 0
  --print puzzle

  hClose handle
