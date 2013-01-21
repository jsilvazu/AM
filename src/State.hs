-----------------------------------------------------------------------------
--
-- Module      :  State
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module State (
                  FIOut(..)
                  ,Stk(..)
                  ,ValStk(..)
                  ,emptyStk
                  ,addStk
                  ,bckStk
                  ,splitStk
                  ,foHasErrs
                  ,emptyFo
                  ,Gam
                  ,emptyGamma
                  ,addVar
                  ,ValGam(..)
                  ,valGamLookup
                  ,valGamLookupVar1
                  ,chngVar
                  ,gamma2Var
) where

import Data.List

data FIOut = FIOut {
                    foe    :: ValStk,
                    fos    :: Gam String Int,
                    foErrL :: String
                   }

emptyFo = FIOut  {
                    foe    = emptyStk,
                    fos    = emptyGamma,
                    foErrL = []
                 }


data Stk
        = ConsInt Int
        | ConsBool Bool
--        deriving Show

instance Show Stk where
    show (ConsBool b) = show b
    show (ConsInt i)  = show i




emptyStk :: ValStk
emptyStk = []

addStk :: Stk -> ValStk -> ValStk
addStk x xs = x:xs

splitStk :: ValStk -> ValStk -> ValStk
splitStk xs ys = xs ++ ys

bckStk :: ValStk -> (Stk, ValStk)
bckStk (x:xs) = (x, xs)

type ValStk = [Stk]

--instance Show (ValStk) where
--    show (x:xs) =  "(HNm " ++ show x ++ ")"

foHasErrs :: FIOut -> Bool
foHasErrs = not.null.foErrL

type Vari k v = [(k,v)]

newtype Gam k v = Gam (Vari k v)

type ValGam = Gam String Int

gamma2Var :: ValGam -> Vari String Int
gamma2Var (Gam l) = l

emptyGamma :: Gam s v
emptyGamma = Gam []

splitVar :: Gam String Int -> Gam String Int -> Gam String Int
splitVar (Gam l1) (Gam l2) = Gam (l1 ++ l2)

addVar :: String -> Int -> Gam String Int -> Gam String Int
addVar s v (Gam y) =  Gam ((s,v) : y)

remVar :: String -> Gam String Int -> Gam String Int
remVar s (Gam y) = Gam (deleteBy (\(v1,_) (v2,_) -> v1 == v2) (s,0) y)

chngVar :: String -> Int -> Gam String Int -> Gam String Int
chngVar s v l =  splitVar (Gam [(s,v)]) (remVar s l)

gamLookup :: Ord s => s -> Gam s v -> Maybe v
gamLookup s (Gam l) = lookup s l

valGamLookup :: String -> ValGam -> Maybe Int
valGamLookup = gamLookup

valGamLookupVar :: String -> ValGam -> (Int,String)
valGamLookupVar n g = case valGamLookup n g of
                         Nothing    -> (0, n)
                         Just    l  -> (l, n)

valGamLookupVar1 :: String -> ValGam -> Maybe (Int,String)
valGamLookupVar1 n g = case valGamLookup n g of
                         Nothing    -> Nothing
                         Just    l  -> Just (l, n)

--instance Show (Gam String Int) where
--    show (Gam s i) =  "(" ++ s ++ show ")"
