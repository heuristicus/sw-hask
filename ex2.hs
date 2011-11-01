import Data.List
-- 1
productList :: [Int] -> Int
productList [] = 1
productList (x:xs) = x * productList xs

-- or

productList2 :: [Int] -> Int
productList2 x = foldr (*) 1 x

-- 2
myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand xs

-- or

myand2 :: [Bool] -> Bool
myand2 x = foldr (&&) True x

-- 3
concatList :: [[Int]] -> [Int]
concatList [] = []
concatList (x:xs) = x ++ concatList xs

-- or
concatList2 :: [[Int]] -> [Int]
concatList2 x = foldr (++) [] x

-- 4
--while pred [x] = pred x ? ([x], False)
--while pred (x:xs) = 

-- 5
iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = insDesc x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys) | x <= y = x:y:ys
             | otherwise = y:ins x ys

minList :: [Int] -> Int
minList x = head (iSort x)

maxList :: [Int] -> Int
maxList x = last (iSort x)

-- 6
minList2 :: [Int] -> Int
minList2 [] = 0
minList2 [x] = x
minList2 (x:xs) = minList2 (filter (<x) xs)

--maxList2 :: [Int] -> Int

-- 7
insDesc :: Int -> [Int] -> [Int]
insDesc x [] = [x]
insDesc x (y:ys) | x >= y = x:y:ys
                 | otherwise = y:insDesc x ys

-- 8
insMult :: Int -> [Int] -> [Int]
insMult x [] = [x]
insMult x (y:ys) | x < y = x:y:ys
                 | x == y = x:ys
                 | otherwise = y:ins x ys

-- 9
memberNum :: [Int] -> Int -> Int
memberNum xs x | xs == [] = 0
               | otherwise = length (filter (\y -> y == x) xs)

-- 10
member :: [Int] -> Int -> Bool
member xs x = (memberNum xs x) >= 1

-- 11
member2 :: [Int] -> Int -> Bool
member2 xs x = length (filter (\y -> y == x) xs) /= 0

-- 12
rev2 (x:xs) | length (x:xs) == 2 = (xs !! 0) : [x]
            | otherwise = (x:xs)

-- 13
position :: Int -> [Int] -> Int
position i xs = phelp i xs 

phelp :: Int -> [Int] -> Int
phelp ct [] = 0
phelp ct (x:xs) | ct == 1 = x
                | otherwise = phelp (ct - 1) xs