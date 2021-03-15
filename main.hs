import System.IO
import Control.Monad

-- Get puzzle dimension
dimension :: [t]-> t
dimension (n:_) = n

convertStrInt :: [Char] -> Int
convertStrInt str = read str :: Int

slice xs i k | i > 0 = take (k - i) $ drop (i ) xs

main = do
  -- Aloca e retornar um novo manipulador para gerenciar o arquivo
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let list = lines contents
  --print list

  -- Dimensão da Matriz
  let n = convertStrInt (dimension list)
  -- Numero de Regiões
  let numOfRegions = convertStrInt (dimension (slice list 1 2))

  print n
  print numOfRegions

  hClose handle
