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
fact x | x > 0 = foldl (*) 1 [1..x]
       | otherwise = error "integer must be positive"

-- 4
--comb :: Integer -> Integer -> Fractional
comb n m | n < m = error "n cannot be less than m"
         | otherwise = div (fact n) ((fact m) * (fact (n-m)))

-- 5
mygcd :: Integer -> Integer -> Integer
mygcd 0 0 = error "Both numbers cannot be zero"
mygcd n 0 = n
mygcd n m = mygcd m (mod n m)