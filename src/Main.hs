module Main where

import UU.Parsing as Par
import UU.Scanner
import UU.Pretty
import State
import SemExpr

import System.Environment(getArgs)

pExpr  =
        pPush <|>
        pSecu <|>
        pAdd  <|>
        pMul  <|>
        pSub  <|>
        pTt   <|>
        pFf   <|>
        pEq   <|>
        pLe   <|>
        pAnd  <|>
        pNeg  <|>
        pFet  <|>
        pStr  <|>
        pNop  <|>
        pBran

pPush  = Push <$  pKey "PUSH"   <*> pInteger
pSecu  = Secu <$  pKey ":"      <*> pExpr <*> pExpr
pAdd   = Add  <$  pKey "ADD"
pMul   = Mul  <$  pKey "MULT"
pSub   = Sub  <$  pKey "SUB"
pTt    = Tt   <$  pKey "TRUE"
pFf    = Ff   <$  pKey "FALSE"
pEq    = Eq   <$  pKey "EQ"
pLe    = Le   <$  pKey "LE"
pAnd   = And  <$  pKey "AND"
pNeg   = Neg  <$  pKey "NEG"
pFet   = Fetc <$  pKey "FETCH"  <*> pVarid
pStr   = Stor <$  pKey "STORE"  <*> pVarid
pNop   = Noop <$  pKey "NOOP"
pBran  = Bran <$  pKey "BRANCH" <* pKey "(" <*> pExpr <* pKey ";" <*> pExpr <* pKey ")"

pRoot  = RootAbs <$> pExpr

--pInt   = Num . read <$> pInteger

-------------------------------------------------------------

lmbdScanTxt :: String -> [Token]
lmbdScanTxt = lmbdScan (Pos 0 0 "")

lmbdScanFl :: FilePath -> String -> [Token]
lmbdScanFl f s = lmbdScan (Pos 1 0 f) s

process :: FilePath -> IO ()
process f = do
  s <- readFile f
  t pRoot (lmbdScan (Pos 0 0 "") s)

t p inp -- test parser p on input inp :t HsName
  = do  c <- parseIO p inp
        let   inh = Inh_RootAbs {}
              syn = wrap_RootAbs (sem_RootAbs c) inh
              fo  = fo_Syn_RootAbs syn
        if foHasErrs fo
           then
                putStrLn $ "Errores tiene el programa" ++ (show $ foErrL fo)
           else
                let inm = fos fo
                    insk = foe fo
--                    (x:xs) = insk
                    inst = gamma2Var inm
                in
--                putStrLn x
                putStrLn $ "State: " ++ (show $ inst)

use :: String
use = "Use: wh <path>"


mail = do
        args <- getArgs
        if length args /= 1
         then
            print use
         else
            print use--process (head args)

-- IO <$
main = do
          let
            s = ": : PUSH 1 STORE x FETCH x"
--            s = ": : : TRUE BRANCH( TRUE ; FALSE) TRUE AND"
--            s = ": : : : : : : : : PUSH 1 PUSH 3 PUSH 3 ADD MULT PUSH 5 EQ TRUE AND NEG"
--            s = "; ; PUSH 1 PUSH 2 MULT"
--          t pRoot (lmbdScanTxt s )
          a <- parseIO pExpr (lmbdScanTxt s)
          print a

kywrdtxt = ["PUSH", "ADD", "MULT", "SUB", "TRUE",
            "FALSE", "EQ", "LE", "AND", "NEG",
            "FETCH", "STORE", "NOOP", "BRANCH"]
kywrdops = [ "==", "<=",  "=", "+", ":=", "-", ":", "*", ";"]
spcchrs  = "()[]{}|"
opchrs   = "-:=>+,;*<="
lmbdScan = scan kywrdtxt kywrdops spcchrs opchrs





















