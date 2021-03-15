import System.IO
import Control.Monad

-- Get puzzle dimension
dimension :: [t]-> t
dimension (n:_) = n

convertStrInt :: [Char] -> Int
convertStrInt str = read str :: Int

-- Slice list
slice xs i k | i > 0 = take (k - i) $ drop (i) xs

main = do
  -- Aloca e retornar um novo manipulador para gerenciar o arquivo
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

  print n
  print numOfRegions
  print numOfStartingNumbers
  print regions
  print startingNumbersAndPositions

  hClose handle
