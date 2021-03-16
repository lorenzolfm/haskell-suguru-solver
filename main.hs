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
  numberOfGroups <- getInputData listOfInputs 1
  -- Número de posições pré-preenchidas
  numOfInitiallyFilledCells <- getInputData listOfInputs 2

  print matrixSize
  print numberOfGroups
  print numOfInitiallyFilledCells




  ---- Regiões
  --let regions = slice list 3 4

  ---- Números já preenchidos e suas posições
  --let startingNumbersAndPositions = slice list (4) (4 + numOfStartingNumbers)

  ----print n
  ----print numOfRegions
  ----print numOfStartingNumbers
  ----print regions
  ----print startingNumbersAndPositions
  ---- Fim IO

  ---- Estruturação de dados

  ---- Matriz do jogo
  --let puzzle = matrix n n 0
  ----print puzzle

  --hClose handle
