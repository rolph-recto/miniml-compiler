{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module MiniML.CPS (
  CExp(..), toCPS, runToCPS
) where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Text as T

import MiniML.Util
import MiniML.AST


data CPrimOp = CAdd | CMul | CSub | CDiv deriving (Show)

data CExp = CVar Var
          | Label Var
          | CInt Int
          | CReal Double
          | CStr T.Text
          | CRecord [(CExp, AccessPath)] Var CExp
          | CSelect Int CExp Var CExp
          | COffset Int CExp Var CExp
          | CApp CExp [CExp]
          | CFix [(Var, [Var], CExp)] CExp
          | CSwitch CExp [CExp]
          | CPrim CPrimOp [CExp] [(Var, CExp)]
          deriving (Show)

type CPS a = ExceptT T.Text (State Int) a

toCPrimOp :: LPrimOp -> CPrimOp
toCPrimOp p = case p of
  LAdd -> CAdd
  LMul -> CMul
  LSub -> CSub
  LDiv -> CDiv
  _    -> CDiv -- TODO fix this later


newvar :: CPS Int
newvar = do
  n <- get
  put (n+1)
  return n

toCPS :: LExp -> (CExp -> CPS CExp) -> CPS CExp
toCPS lexp c = case lexp of
  LVar var -> c (CVar var)
   
  LReal r -> c (CReal r)

  LInt i -> c (CInt i)

  LStr s -> c (CStr s)

  LRecord [] -> c (CInt 0)

  LRecord rs -> do
    x <- newvar
    xcps <- c (CVar x)
    let newrec a = return $ CRecord (map (\v -> (v, OFFp 0)) a) x xcps
    toRecordCPS rs newrec
    where toRecordCPS rs c = do
            let go (r:rs) w = toCPS r (\v -> go rs (v:w))
                go [] w     = c (reverse w)
            go rs []

  LSelect i rec -> do
    x <- newvar
    xcps <- c (CVar x)
    toCPS rec (\v -> return $ CSelect i v x xcps)

  LApp (LPrim p) arg -> do
    x <- newvar
    let p' = toCPrimOp p
    xcps <- c (CVar x)
    toCPS arg (\v -> return $ CPrim p' [v] [(x, xcps)])

  LPrim p -> do
    x <- newvar
    toCPS (LFun x (LApp (LPrim p) (LVar x))) c

  LFun f arg -> do
    fname <- newvar
    k <- newvar
    argCPS <- toCPS arg (\v -> return $ CApp (CVar k) [v])
    fcps <- c (CVar fname)
    return $ CFix [(fname, [f, k], argCPS)] fcps

  LApp f arg -> do
    x <- newvar
    r <- newvar
    fcps <- toCPS f (\a -> toCPS arg (\b -> return $ CApp a [b, CVar r]))
    xcps <- c (CVar x)
    return $ CFix [(r, [x], xcps)] fcps

  LFix fbinds expr -> do
    fcps <- go fbinds
    ecps <- toCPS expr c
    return $ CFix fcps ecps
    where go []                = return []
          go ((h,LFun v b):ls) = do
            w <- newvar
            fcps <- toCPS b (\z -> return $ CApp (CVar w) [z])
            tl <- go ls
            return $ (h, [v,w], fcps):tl
          go _ = throwError "Expected function expression in LFix binding"
            
  LCon con e -> do
    let concps = conToCPS con e
    toCPS concps c
    where conToCPS con expr
            | Constant i <- con   = LInt i
            | Tagged i <- con     = LRecord [expr, LInt i]
            | Transparent <- con  = expr
            | Transb <- con       = expr
            | Transu <- con       = expr
                  
runToCPS :: LExp -> Either T.Text CExp
runToCPS lexp =
  let init = (getMaxVar lexp) + 1 in
  evalState (runExceptT (toCPS lexp return)) init

