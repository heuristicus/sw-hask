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