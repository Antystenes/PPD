-- Piotr Radwan 372185
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances#-}
module Mojzbior where

import Control.Monad
import Control.Arrow
import System.Random

iloczyn :: Eq a => [a] -> [a] -> [a]
iloczyn = filterM ((or .) . map . (==))

suma :: Eq a => [a] -> [a] -> [a]
suma = (foldr (\x acc -> if x `elem` acc then acc else x:acc) [] .) . (++)

roznica :: Eq a => [a] -> [a] -> [a]
roznica = flip (filter . flip ((not.) . elem))

podzbior :: Eq a => [a] -> [a] -> Bool
podzbior = (null.). roznica

podaj :: IO Int
podaj = putStrLn "PodajLiczbę" >> (read <$> getLine)

zsumuj = ((+) <$> podaj <*> podaj) >>= print

nwd :: Int -> Int -> Int
nwd a b | a `mod` b == 0 = b
        | otherwise      = nwd b $ a `mod` b

nww :: Int -> Int -> Int
nww = (uncurry div.) . (uncurry (liftM2 (,)) . ((*) &&& nwd))

nwdinww = uncurry (liftM2 (,)) . (nwd &&& nww) <$> podaj <*> podaj >>= print

inic = ((("Twoje inicjały to: " ++).).(. ((' ' :) . return)) . (:)) <$> (putStrLn "podaj imie:" >> head <$> getLine) <*> (putStrLn "podaj nazwisko:" >> head <$> getLine) >>= putStrLn

gra = do
  x <- randomRIO (0,99)
  let petla n =
        if n > 10
        then print $ "Hehe wybrałem: " ++ show x
        else
          do y <- podaj
             if y > x
             then putStrLn "Wygrałeś"
             else putStrLn "Wybrałem większą." >> petla (n+1)
  petla 0

-- Zad 6.
--Klasa wieloparametryzowana która pozwala na dodawanie do siebie różnych typów numerycznych

class (Num a, Num b) => MultiNumeric a b where
  (<+.>) :: a -> b -> b
  (<*.>) :: a -> b -> b

--Przykładowa instancja dla wszystkich typów Całkowitych jako pierwszy element sumy i dowolny typ ułamkowy jako drugi

instance (Integral a, Fractional b) => MultiNumeric a b where
  (<+.>) = (+) . fromIntegral
  (<*.>) = (+) . fromIntegral

--Przykładowe wywołanie:
--(4 :: Int) <+.> (4.5 :: Float)
