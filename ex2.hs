-- 1
productList :: [Int] -> Int
productList [] = 1
productList (x:xs) = x * productList xs

-- or

productList2 :: [Int] -> Int
productList2 x = foldr (*) 1 x

-- 2