import Data.List

-- 1
sotls :: Int -> Int -> Int -> Int
sotls x y z = sum (map square (delete (minimum [x,y,z]) [x,y,z]))

square x = x * x

-- 2