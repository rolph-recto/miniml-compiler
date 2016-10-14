{-# LANGUAGE GADTs #-}

module MiniML.AST (
  LExp(..), LPrimOp(..), ConRep(..), getVars, getMaxVar
) where

import qualified Data.Text as T
import MiniML.Util

-- this is called the "lambda language in Appel (Ch. 4)

data ConRep = Tagged Int
            | Constant Int
            | Transparent
            | Transu
            | Transb
            | Ref
            -- Variable constructors are used for exception handling
            -- | Variable Var AccessPath
            -- | VariableC Var AccessPath
            deriving (Show)

data Con = DataCon ConRep
         | IntCon Int
         | RealCon Double
         | StrCon T.Text
         deriving (Show)

data LPrimOp = LAdd | LMul | LSub | LDiv | CallCC | Throw deriving (Show)

-- expressions
data LExp = LVar Var
          | LFun Var LExp
          | LFix [(Var,LExp)] LExp
          | LApp LExp LExp
          | LInt Int
          | LReal Double
          | LStr T.Text
          | LSwitch LExp [ConRep] [(Con, LExp)] (Maybe LExp)
          | LCon ConRep LExp
          | LDecon ConRep LExp
          | LRecord [LExp]
          | LSelect Int LExp
          | LPrim LPrimOp
          -- | Raise LExp
          -- | Handle LExp LExp
          deriving (Show)

getVars :: LExp -> [Var]
getVars lexp = case lexp of
  LVar v -> [v]

  LFun arg body -> arg:(getVars body)

  LFix binds exp -> 
    (map fst binds) ++ (concatMap getVars (map snd binds)) ++ (getVars exp)

  LApp f arg -> (getVars f) ++ (getVars arg)

  LInt _ -> []

  LReal _ -> []

  LStr _ -> []

  LSwitch e _ branches def ->
    (getVars e) ++ (concatMap getVars (map snd branches)) ++ (maybe [] getVars def)

  LCon _ e -> getVars e

  LDecon _ e -> getVars e

  LRecord rs -> concatMap getVars rs

  LSelect _ r -> getVars r

  LPrim _ -> []

getMaxVar :: LExp -> Var
getMaxVar = maximum . getVars
