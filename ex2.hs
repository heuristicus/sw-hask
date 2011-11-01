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
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys) | x <= y = x:y:ys
             | otherwise = y:ins x ys

minList :: [Int] -> Int
minList x = head (iSort x)

maxList :: [Int] -> Int
maxList x = last (iSort x

-- 6