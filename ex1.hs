import Data.List

-- 1
sotls :: Integer -> Integer -> Integer -> Integer
sotls x y z = sum (map square (delete (minimum [x,y,z]) [x,y,z]))

square x = x * x

-- 2
sumsq :: Integer -> Integer
sumsq x | x > 0 = sum (map square [1..x])
        | otherwise = error "integer must be positive"

-- 3
fact :: Integer -> Integer
fact x | x > 0 = product [1..x]
       | otherwise = error "integer must be positive"