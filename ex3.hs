import Data.List

-- 1
--area a b c d p q | if valid a b p == True = sqrt (4*p^2*q^2-(b^2+d^2-a^2-c^2)^2)
--                 | otherwise = error "Invalid quadrilateral"

-- 2
fd :: Int -> Int
fd i = round ((1/rfv) * (((1+rfv)/2)^i-((1-rfv)/2)^i)) where rfv = sqrt 5
                                                             
-- 3
--whatday :: Int -> Int -> Int -> Int -> Int
--march = 1, feb=12
whatday day month year cent = mod (truncate a) 7 where a = floor (2.6 * month - 0.2) + day + year + floor (year/4) + floor (cent/4) - 2 * 19
                                                       
-- 4
sumHarmonic i = harmonic i 0 where 
  harmonic 0 x = x
  harmonic i x = harmonic (i - 1) (x+(1/i))