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

-- Definição de novos tipos
type Position = (Int, Int)
type Region = [Position]

-- Converte uma string em posição
-- Recebe uma string "x,y" e converte em (x,y)
convertStrPos :: [Char] -> Position
convertStrPos str = (read [str !! 0] :: Int, read [str !! 2] :: Int)

-- Adiciona uma posição a uma lista de posições (região)
appendPos :: [Position] -> Position -> [Position]
appendPos list pos = pos : list

-- Adiciona uma região a uma lista de regiões
-- Exemplo: adiciona [(x,y),(a,b)] a uma lista 
appendReg :: [Region] -> Region -> [Region]
appendReg list reg = reg : list

-- Converte uma string em região
-- Recebe "0,0 0,1 0,2 1,0" e retorna [(0,0),(0,1),(0,2),(1,0)]
convertStrReg :: Int -> [Char] -> Region -> Region
convertStrReg (-1) _ reg = return reg!!0
convertStrReg n str reg = do
    let test = drop (n-3) str
    let pos = convertStrPos (take 3 test)
    let aux = appendPos reg pos
    convertStrReg (n-4) str aux
    

-- Slice list
slice xs i k | i > 0 = take (k - i) $ drop (i) xs

matrix :: Int -> Int -> a -> [[a]]
matrix x y = replicate y . replicate x
