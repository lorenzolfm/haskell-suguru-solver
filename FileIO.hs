module FileIO (getFilePath, getFileContents, getInputData, dimension, convertStrInt, slice, matrix) where

import System.IO
import System.Environment

-- Retorna o nome do arquivo de entrada passado como argumento.
getFilePath = do
    -- getArgs retorna uma lista com todos os argumentos passados como argumento
  argumentsList <- getArgs
    -- lista !! n retorna o elemento na posição n da lista.
  let fileName = argumentsList !! 0
  return(fileName)

getFileContents filePath = do
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  let list = lines contents
  return(list)

getInputData list n = do
  return(convertStrInt (list !! n))

-- Dimensão do tabuleiro
dimension :: [t]-> t
dimension (n:_) = n

convertStrInt :: [Char] -> Int
convertStrInt str = read str :: Int

-- Slice list
slice xs i k | i > 0 = take (k - i) $ drop (i) xs

matrix :: Int -> Int -> a -> [[a]]
matrix x y = replicate y . replicate x
