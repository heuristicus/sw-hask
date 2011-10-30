import Data.List

-- 1
sotls :: Int -> Int -> Int -> Int
sotls x y z = sum (map square (delete (minimum [x,y,z]) [x,y,z]))

square x = x * x

-- 2
sumsq :: Int -> Int
sumsq x | x > 0 = sum (map square [1..x])
        | otherwise = error "integer must be positive"