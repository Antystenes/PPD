-- Piotr Radwan

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Parser (
               -- * Klasyczny Rachunek zdań
               JKRZ(..)
               -- * Parser
              , ArrParser(..)
              , parse
              , pAlt
              , pSym
               -- * Ewaluacja
              , eval
               -- * Sprawdzanie tautologii
              , parseAndCheckTautology ) where

import Prelude hiding ((.))
import Control.Category
import Control.Arrow
import Control.Monad
import Control.Applicative
import Data.List (nub)

letters :: String
letters = ['a'..'z']++['A'..'Z']

numbers :: String
numbers = ['0','1']

-- | Struktura danych do reprezentowania wyrażeń Języka Klasycznego Rachunku Zdań
data JKRZ = -- | Alternatywa
            Alt JKRZ JKRZ
          | Con JKRZ JKRZ --  Koniunkcja
          | Imp JKRZ JKRZ --  Implikacja
          | Eq  JKRZ JKRZ --  Równoważność
          | Neg JKRZ      --  Negacja
          | Var String    --  Zmienna Logiczna
          | Val Bool      --  Wartość Logiczna
          deriving Show

-- Parser

newtype ArrParser a b = P {runParser :: [a] -> [(b,[a])]}

instance Functor (ArrParser a) where
  fmap f (P p) = P (fmap (first f) . p)

instance Applicative (ArrParser a) where
  pure x          = P $ \str -> [(x,str)]
  (P f) <*> (P x) = P $ f >=> (\(a,in1) -> first a <$> x in1)

instance Alternative (ArrParser a) where
  empty               = P $ const []
  (<|>) (P p1) (P p2) = P $ \inp -> p1 inp ++ p2 inp

instance Monad (ArrParser a) where
  return    = pure
  P f >>= g = P $ f >=> (\(a,in1) -> runParser (g a) in1)

pLetter :: ArrParser Char Char
pLetter = pAlt letters

pNum :: ArrParser Char Bool
pNum = (\x -> if x == '0' then False else True) <$> pAlt numbers

pAlt :: Eq a => [a] -> ArrParser a a
pAlt symTable = P $ \l -> case l of
  []     -> []
  (x:xs) -> [(x,xs) | x `elem` symTable ]

pSym :: Eq a => a -> ArrParser a a
pSym c = P $ \l -> case l of
  []     -> []
  (x:xs) -> [(x,xs) | x == c]

pPlus :: Alternative f => f a -> f [a]
pPlus p = (:) <$> p <*> pPlus p <|> (:[]) <$> p

pWithBrackets :: ArrParser Char a -> ArrParser Char a
pWithBrackets p = pSym '(' >> p >>= \x -> pSym ')' >> return x

optBrackets :: ArrParser Char a -> ArrParser Char a
optBrackets p = (pSym '(' <|> return 'a') >> p >>= \x -> (pSym ')' <|> return 'a') >> return x

pStar :: Alternative f => f a -> f [a]
pStar p = pure [] <|> pPlus p

parseSentence :: ArrParser Char JKRZ
parseSentence = optBrackets (pWhSp >> (pVar <|> (const Neg <$> pSym '~' <*> pArg) <|> pAl <|> pCon <|> pImp <|> pEq) >>= \x -> pWhSp >> return x )
  where pVar       = Var <$> pPlus pLetter
        pVal       = Val <$> pNum
        pArg       = pVar <|> pVal <|> pWithBrackets parseSentence
        pWhSp      = pStar $ pSym ' '
        pBinOp o p = flip ($) <$> pArg <*> (pWhSp >> p >> return o) <*> (pWhSp >> pArg)
        pAl        = pBinOp Alt $ pSym '|' <|> pSym '∨'
        pCon       = pBinOp Con $ pSym '&' <|> pSym '∧'
        pImp       = pBinOp Imp $ (pSym '=' >> pSym '>') <|> pSym '→'
        pEq        = pBinOp Eq  $ (pSym '<' >> pSym '=' >> pSym '>') <|> pSym '='

getVars :: JKRZ -> [String]
getVars = nub . getVars'
  where getVars' (Con x y) = getVars x ++ getVars y
        getVars' (Eq  x y) = getVars x ++ getVars y
        getVars' (Imp x y) = getVars x ++ getVars y
        getVars' (Alt x y) = getVars x ++ getVars y
        getVars' (Neg x)   = getVars x
        getVars' (Var x)   = [x]
        getVars' _         = []

-- | Wywołanie funkcji "parse" zwróci kompletne parsy stringa podanego jako argument
-- Przykłady poprawnych zdań to:
--  p → (q ∨ r)
-- po wywołaniu < parse "p → (q ∨ r)" > orzymujemy:
--   [(Imp (Var "p") (Alt (Var "q") (Var "r")),"")]
-- inne przykłady zdań wraz z wynikami:
--  p = (q → (r∧q))
--   [(Eq (Var "p") (Imp (Var "q") (Con (Var "r") (Var "q"))),"")]
--  (~p) = (q →        (              q          ∨           g    )     )
--   [(Eq (Neg (Var "p")) (Imp (Var "q") (Alt (Var "q") (Var "g"))),"")]
--  ∧∨
--   []

parse = filter (null.snd). runParser parseSentence

eval (Eq x y) = (ex && ey) || not (ex || ey)
  where ex = eval x
        ey = eval y
eval (Imp x y) = not (eval x) || eval y
eval (Alt x y) = eval x || eval y
eval (Con x y) = eval x && eval y
eval (Neg x)   = not . eval $ x
eval (Val x)   = x

subVal formula k@(name, value) = case formula of
  Eq x y -> Eq (subVal x k) (subVal y k)
  Imp x y -> Imp (subVal x k) (subVal y k)
  Alt x y -> Alt (subVal x k) (subVal y k)
  Con x y -> Con (subVal x k) (subVal y k)
  Neg x   -> Neg $ subVal x k
  Var x   -> if name == x then Val value else Var x
  x       -> x

subVals :: JKRZ -> [(String,Bool)] -> JKRZ
subVals = foldl subVal

checkIfTautology x = foldr ((&&).eval . subVals x) True valLists
  where valLists = map (zip vars) $ binSeqs $ length vars
        vars     = getVars  x
        binSeqs n= replicateM n [True,False]

-- | Parses expression and checks, if it is thautology. Returns boolean value.

parseAndCheckTautology x = case parse x of
  [] -> False
  (x:_) -> checkIfTautology . fst $ x
