import System.IO
import Control.Monad

-- Get puzzle dimension
dimension :: [t]-> t
dimension (n:_) = n

main = do
  -- Aloca e retornar um novo manipulador para gerenciar o arquivo
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let arr = words contents

  -- Get Puzzle dimension
  let n_str = dimension arr
  print n_str
  let n = (read n_str :: Int)
  print n

  -- Fecha o handle
  hClose handle
