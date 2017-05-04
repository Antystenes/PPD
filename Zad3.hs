module Zad3 where

import Control.Monad
import Data.List
import Control.Arrow
import Data.Monoid
-- Zad 1
powerlist :: [a] -> [[a]]
powerlist = filterM (const [False, True])
-- Zad 2
iloczyn :: Eq a => [a] -> [a] -> [a]
iloczyn = filterM ((or .) . map . (==))
-- Zad 3
suma :: Eq a => [a] -> [a] -> [a]
suma = (foldr (\x acc -> if x `elem` acc then acc else x:acc) [] .) . (++)
-- Zad 4
--a) 8/2 -> 24/4 -> 12/6 -> 6/2
--b) 5==5 && True -> 3>2 && 5==5 && True -> 1>2 && 3>2 && 5==5 && True
--c) max 11 18 -> max 55 18 -> max 4 55 -> max 12 55 -> max 6 55 -> max 3 55
--d) max 11 81 -> max 55 81 -> max 4 88 -> max 12 88 -> max 6 88 -> max 3 88
--e) (6+54)/2 -> (10+30)/2 -> (4+20)/2 -> (24+24)/2
--f) (54+2)/2 -> (28+4)/2 -> (16+10)/2 -> (13+6)/2
--g) 64/4 -> 16/2 -> 8/4
--h) 2 * 8 + 1 -> 17 * 2 + 2 -> 36 * 2 + 3
-- Zad 5
nalezy :: Eq a => a -> [a] -> Bool
nalezy = (or .) . map . (==)
-- Zad 6
mapr :: (a -> b) -> [a] -> [b]
mapr f = foldr ((:) . f) []

mapl :: (a -> b) -> [a] -> [b]
mapl f = foldl (flip (flip (++) . return . f)) []
-- Zad 7
ostatni :: [a] -> a
ostatni = foldr1 (flip const)

pierwszy :: [a] -> a
pierwszy = foldr1 const

maksimum :: Ord a => [a] -> a
maksimum = foldr1 max

-- Zad 8
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y) : zip xs ys

myunzip = map fst &&& map snd

-- Zad 9
y = \f -> f (y f)

silnia = y (\t n -> if n == 1 then 1 else n * (t n))

fibonacci = y (\t n -> case n of
                  0 -> 1
                  1 -> 1
                  _ -> t (n-1) + t (n-2))

cartesian = \l1 l2 -> l1 >>= \x -> map ((,) x) l2

abelian :: (Eq m, Monoid m) => m -> m -> Bool
abelian = \x -> (<> x) &&& (x <>) >>> uncurry (==)
