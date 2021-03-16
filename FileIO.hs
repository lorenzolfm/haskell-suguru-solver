module FileIO (dimension, convertStrInt, slice, matrix) where

-- Dimensão do tabuleiro
dimension :: [t]-> t
dimension (n:_) = n

convertStrInt :: [Char] -> Int
convertStrInt str = read str :: Int

-- Slice list
slice xs i k | i > 0 = take (k - i) $ drop (i) xs

matrix :: Int -> Int -> a -> [[a]]
matrix x y = replicate y . replicate x
