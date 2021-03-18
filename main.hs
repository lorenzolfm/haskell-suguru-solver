module Main (main) where

import FileIO
import System.IO

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (x:xs)
    | (x == ' ') = removeSpaces xs
    | otherwise = x : removeSpaces (xs)

removeComma :: String -> String
removeComma [] = []
removeComma (x:xs)
    | (x == ',') = removeComma xs
    | otherwise = x : removeComma (xs)

converToChar :: String -> [Char]
converToChar str = str

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

  -- Regiões (lista de strings)
  let listOfGroupStrings = slice listOfInputs 3 (4 + numOfGroups - 1)


  -- Células pré-preenchidas (lista de strings)
  let listOfInitiallyFilledCellsStrings = slice listOfInputs (3 + numOfGroups) (4 + numOfGroups + numOfInitiallyFilledCells - 1)
  ---- Fim IO

  ---- Estruturação dos dados

  print listOfGroupStrings
