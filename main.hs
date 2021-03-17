module Main (main) where

import FileIO
import System.IO
--import Control.Monad

main = do
  -- FileIO

  -- Nome do arquivo passado como argumento
  filePath <- getFilePath
  listOfInputs <- getFileContents filePath

  -- Dimensão da Matriz
  matrixSize <- getInputData listOfInputs 0
  -- Número de Grupos
  numOfGroups <- getInputData listOfInputs 1
  -- Número de posições pré-preenchidas
  numOfInitiallyFilledCells <- getInputData listOfInputs 2

  print matrixSize
  print numOfGroups
  print numOfInitiallyFilledCells

  -- Regiões (lista de strings)
  let listOfGroupStrings = slice listOfInputs 3 (4 + numOfGroups - 1)
  print listOfGroupStrings

  -- Células pré-preenchidas (lista de strings)
  let listOfInitiallyFilledCellsStrings = slice listOfInputs (3 + numOfGroups) (4 + numOfGroups + numOfInitiallyFilledCells - 1)
  print listOfInitiallyFilledCellsStrings
  ---- Fim IO

  ---- Estruturação de dados

  ---- Matriz do jogo
  --let puzzle = matrix n n 0
  ----print puzzle

  --hClose handle
