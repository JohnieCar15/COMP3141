module Ex03 where

import Data.List
import Test.QuickCheck

-- Consider the following six properties of the nub function:
--
-- 1. nub (nub xs) == nub xs
-- 2. nub (x : nub xs) == nub (x : xs)
-- 3. nub (xs ++ nub ys) == nub (xs ++ ys)
-- 4. nub (x : x : xs) == nub (x : xs)
-- 5. nub (xs ++ [x] ++ xs) = nub (xs ++ [x])
-- 6. nub [x] == [x]

-- Task 1. Implement a dodgy nub function that satisfies
-- equations 1, 2, 3 and 6, but not 4, 5.
-- Give counterexamples to properties 4,5, i.e. values
-- (x,xs) for which equations 4 and 5 do not hold.

dnub1236_eq1 :: [Int] -> Bool
dnub1236_eq1 xs = dnub123456 (dnub123456 xs) == dnub123456 xs

dnub1236_eq2 :: Int -> [Int] -> Bool
dnub1236_eq2 x xs = dnub123456 (x : dnub123456 xs) == dnub123456 (x : xs)

dnub1236_eq3 :: [Int] -> [Int] -> Bool
dnub1236_eq3 xs ys = dnub123456 (xs ++ dnub123456 ys) == dnub123456 (xs ++ ys)

dnub1236_eq4 :: Int -> [Int] -> Bool
dnub1236_eq4 x xs = dnub123456 (x : x : xs) == dnub123456 (x : xs)

dnub1236_eq5 :: Int -> [Int] -> Bool
dnub1236_eq5 x xs = dnub123456 (xs ++ [x] ++ xs) == dnub123456 (xs ++ [x])

dnub1236_eq6 :: Int -> Bool
dnub1236_eq6 x = dnub123456 [x] == [x]

dnub1236_eq :: Int -> [Int] -> [Int] -> Bool
dnub1236_eq x xs ys =
    dnub1236_eq1 xs && dnub1236_eq2 x xs && dnub1236_eq3 xs ys && dnub1236_eq6 x && dnub1236_eq4 x xs && not (dnub1236_eq5 x xs)

dnub1236 :: (Eq a) => [a] -> [a]
dnub1236 [] = []
dnub1236 (x:xs) = x : nub xs


dnub1236_c4 :: (Int, [Int]) -- (x,xs) failing eqn 4
dnub1236_c4 = (0, [])

dnub1236_c5 :: (Int, [Int]) -- (x,xs) failing eqn 5
dnub1236_c5 = (1, [0])

-- Task 2. Implement a dodgy nub function that satisfies
-- equations 1, 2, 3, 4, 5, but not 6.
-- Give counterexamples.

dnub12345 :: (Eq a) => [a] -> [a]
dnub12345 xs = []

dnub12345_c6 :: Int
dnub12345_c6 = 1

-- Task 3. Implement a dodgy nub function that satisfies
-- equations 4, 5, 6, but not 1,2,3.
-- Give counterexamples.

dnub456 :: (Eq a) => [a] -> [a]
dnub456 xs = reverse $ nub xs

dnub456_c1 :: [Int]
dnub456_c1 = [0,1]

dnub456_c2 :: (Int,[Int])
dnub456_c2 = (0,[1,2])

dnub456_c3 :: ([Int],[Int]) -- (xs,ys)
dnub456_c3 = ([],[0,1])

-- Task 4. Implement a dodgy nub function that satisfies
-- all the equations 1,2,3,4,5,6. (3 marks)

dnub123456 :: (Eq a) => [a] -> [a]
dnub123456 xs = removedups (nub xs)
  where
    removedups [] = []
    removedups [x] = [x]
    removedups (x:y:ys)
      | x /= y = removedups (x:ys)
      | otherwise = x : removedups (y:ys)

-- Show that dnub123456 is dodgy, i.e. give an input xs for which
-- dnub123456 xs /= nub xs.
dnub123456_c :: [Int]
dnub123456_c = [1,2,2,3]

