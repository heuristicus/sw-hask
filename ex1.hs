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

-- 6
--prime :: Integer -> Bool
--prime 1 = True
--prime x | x < 1 = error "no negatives or zero"
--        | otherwise =  map (mygcd x xs) [1..x]

-- 7
perfect :: Integer -> Bool
perfect x = (foldr (+) 0 (divisors x)) == x

divisors :: Integer -> [Integer]
divisors x = filter (\y -> mod x y == 0) [1..(div x 2)]

-- 8
-- No idea.
--coins = [1,2,5,10,20,50,100,200]
--countways :: Integer -> Integer -> Integer
--countways n m | n < 1 || m < 1 = error "unaccepted input"
--              | otherwise = 

-- 9
abundant :: Integer -> Bool
abundant x = (foldr (+) 0 (divisors x)) > x

-- 10
amicable :: Integer -> Integer -> Bool
amicable x y = (foldr (+) 0 (divisors y)) == x && (foldr (+) 0 (divisors x)) == y