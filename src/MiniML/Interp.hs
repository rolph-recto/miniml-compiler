{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module MiniML.Interp (
  interp, runMiniML
) where


import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Monoid

import Control.Monad.Reader
import Control.Monad.Except

import MiniML.AST

type Env = M.Map VarName LExp
type InterpM a = ExceptT T.Text (ReaderT Env IO) a

interp :: LExp -> InterpM LExp
interp exp = case exp of

  ILit _ -> return exp

  RLit _ -> return exp

  SLit _ -> return exp

  Fun _ _ -> return exp

  Fix bindings exp -> do
    expval <- local applyBindings (interp exp)
    return expval
    where applyBindings env = foldr (uncurry M.insert) env bindings 

  Var v -> do
    env <- ask
    case M.lookup v env of
      Nothing -> throwError $ "No binding for variable " <> T.pack (show v)
      Just val -> interp val

  App f arg -> do
    fval <- interp f
    case fval of
      Fun param body -> do
        argval <- interp arg
        local (M.insert param argval) (interp body)

      otherwise -> do
        throwError "Cannot apply argument to a non-function expression"

  Switch x cons branches def -> do
    xval <- interp x
    tryBranches xval branches
    where -- no more branches to try. check for default
          tryBranches (Con _ _) [] = do
            case def of
              Nothing -> throwError "Inexhaustive pattern"
              Just defexpr -> do
                defval <- interp defexpr
                return defval
          tryBranches exp@(Con c body) ((bc,bexpr):bs) = do
            if c == bc
            then do
              bval <- interp bexpr
              return bval
            else
              tryBranches exp bs

  Con rep body ->  do
    bodyval <- interp body
    return $ Con rep bodyval

  Decon dc (Con c body) -> do
    if dc == c
    then do
      bodyval <- interp body
      return $ bodyval
    else throwError "Mismatch of constructors in DECON operator"

  Decon c _ -> do
    let errmsg = "Cannot deconstruct expression with constructor "
    let errmsg = errmsg <> T.pack (show c) <> ", expected " <> T.pack (show c)
    throwError errmsg

  Record entries -> do
    Record <$> forM entries interp

  Select 0 (Record (r:_)) -> do
    rval <- interp r
    return rval

  Select n (Record []) -> do
    throwError "Selection index for record is out of bounds"
  
  Select n (Record (_:rs)) -> do
    interp $ Select (n-1) (Record rs)

  Select _ _ -> do
    throwError "Cannot select from a non-record expression"

  Prim (Add x y) -> interpArithPrimop (+) x y

  Prim (Sub x y) -> interpArithPrimop (-) x y

  Prim (Mul x y) -> interpArithPrimop (*) x y

  Prim (Div x y) -> do
    xval <- interp x
    yval <- interp y
    case (xval, yval) of
      (ILit _, ILit 0) -> do
        throwError "Division by zero (int)"
      (RLit _, RLit 0.0) -> do
        throwError "Division by zero (real)"
      (ILit xint, ILit yint) -> do
        return $ ILit (xint `div` yint)
      (RLit xreal, RLit yreal) -> do
        return $ RLit (xreal / yreal)
      otherwise -> do
        throwError "Expected number arguments for arithmetic primop"

  Prim (Cond pred bthen belse) -> do
    predval <- interp pred
    case predval of
      BLit predbool -> if predbool then interp bthen else interp belse
      otherwise -> do
        throwError "Expected boolean arguments for boolean primop"

  where interpArithPrimop :: (forall n. Num n => n -> n -> n) -> LExp -> LExp -> InterpM LExp
        interpArithPrimop op x y = do
          xval <- interp x
          yval <- interp y
          case (xval, yval) of
            (ILit xint, ILit yint) -> do
              return $ ILit (xint `op` yint)
            (RLit xreal, RLit yreal) -> do
              return $ RLit (xreal `op` yreal)
            otherwise -> do
              throwError "Expected number arguments for arithmetic primop"
                

runMiniML :: LExp -> IO (Either T.Text LExp)
runMiniML expr = runReaderT (runExceptT (interp expr)) M.empty
