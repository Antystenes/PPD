{-# LANGUAGE ExplicitForAll #-}
module Lib where

--  Ćwiczenia pierwsze

f :: forall a. (Ord a, Num a) => a -> a
f x | x > 2 = x^2
    | x > 0 = x-1
    | otherwise = abs x

nwd :: forall a. (Eq a, Integral a) => a -> a -> a
nwd a b | a `mod` b == 0 = b
        | otherwise      = nwd b $ a `mod` b

nww :: forall a. (Eq a, Integral a) => a -> a -> a
nww a b = a * b `div` (nwd a b)

trojkat :: forall a. (Ord a, Num a) => a -> a -> a -> Bool
trojkat a b c = fold3 tri l
  where l = take 5 . cycle $ [a,b,c]
        fold3 :: (a -> a -> a -> Bool) -> [a] -> Bool
        fold3 f l = case l of
          [x,y,z]    -> f x y z
          (x:y:z:ls) -> f x y z && (fold3 f (y:z:ls))
        tri x y z = x + y >= z

stozObj :: forall a. (Floating a) => a -> a -> a
stozObj r h = pi * r * r * h / 3

stozTworz :: (Num a, Floating a, Fractional a) => a -> a -> a
stozTworz r h = sqrt ((r^2) + (h^2))

potega :: forall a. (Ord a, Num a) => a -> a -> a
potega a | a > 0 = \_ -> 0
         | otherwise = \n -> case n of
             0 -> 1
             _ -> a * (potega 1 (n-1))

potegaDokladnie :: forall a. (Eq a, Num a) => a -> a -> a
potegaDokladnie a n = case a of
  0 -> 0
  _ -> f n 1
    where f n acc = if n == 0 then acc else (f (n-1) (a*acc))

fibList :: [Int]
fibList = 1 : 1 : zipWith (+) fibList (tail fibList)

czyDziesiata :: Int -> Bool
czyDziesiata = (==) (fibList !! 9)

czyWFib :: Int -> Bool
czyWFib = flip elem . takeWhile (<100) $ fibList
