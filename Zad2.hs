{-# LANGUAGE RankNTypes #-}
module Zad2 where
import Control.Arrow

--  Ćwiczenia drugie

--    Z.1

prepend :: forall a. a -> [a] -> [a]
prepend = (:)

addAsSnd :: forall a. a -> [a] -> [a]
addAsSnd a (x:xs) = x:a:xs
addAsSnd _ _ = undefined

append :: forall a. [a] -> a -> [a]
append l a = l ++ [a]

--    Z.2

secondL :: forall a. [a] -> a
secondL (_:b:_) = b
secondL _ = undefined

thirdL :: forall a. [a] -> a
thirdL (_:_:b:_) = b
thirdL _ = undefined

penultimate :: forall a. [a] -> a
penultimate = secondL . rev
--    Z.3

rev :: forall a. [a] -> [a]
rev = foldl (flip (:)) []

--    Z.4

swapFL :: forall a. [a] -> [a]
swapFL l@(_:_:_) = (head . rev) l : ((take (length l - 2). tail $ l) ++ [head l])
swapFL _ = undefined


--    Z.5

--       a)
evenSum :: forall a. (Integral a, Ord a) => [a] -> a
evenSum = sum . filter (>0) . filter ((==0) . (`mod` 2))

--       b)
-- flip take [a,b,c,d] 2
dvsblByThree = filter ((==0) . (`mod`3)) . flip take [1..]

nmbrDvsblByThree :: Int -> Int
nmbrDvsblByThree = length . dvsblByThree

--       c)

sumDvsblByThree :: Int -> Int
sumDvsblByThree = sum . dvsblByThree

--    Z.6

evenLength :: [a] -> Bool
evenLength = (==0) . (`mod` 2) . length

--    Z.7
--        a)

sqrlist :: [Int] -> [Int]
sqrlist = map (^2)

--        b)

sqrList2 :: [Int] -> [Int]
sqrList2 = foldr ((:).(^2)) []

--    Z.8

count :: forall a. (Eq a) => a -> [a] -> Int
count = (length .) . filter . (==)

--    Z.9

duplicate :: forall a. a -> Int -> [a]
duplicate = flip replicate

--    Z.10

isPal :: forall a. (Eq a) => [a] -> Bool
isPal = id &&& rev >>> uncurry (==)

--    Z.11

delFst :: forall a. (Eq a) => a -> [a] -> [a]
delFst _ [] = []
delFst a (x:xs) = if x == a then xs else x : (delFst a xs)

--    Z.12

deleteAt :: Integer -> [a] -> [a]
deleteAt = (fmap . fmap) (map snd) (. zip [1..]) . filter . flip ((/=) . fst)

--    Z.13

subset :: forall a. (Eq a) => [a] -> [a] -> Bool
subset = (and .). map . flip elem

--    Z.14

swap :: forall a b.[(a,b)] -> [(b,a)]
swap = map (snd &&& fst)
