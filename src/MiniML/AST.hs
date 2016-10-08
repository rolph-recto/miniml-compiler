{-# LANGUAGE GADTs #-}

module MiniML.AST (
  LExp(..), PrimOp(..), ConRep(..), VarName
) where

import qualified Data.Text as T

-- this is called the "lambda language in Appel (Ch. 4)

type VarName = Int 
data AccessPath where
  OffP :: Int -> AccessPath
  SelP :: Int -> AccessPath

data ConRep where
  -- Undecided   :: ConRep
  Tagged      :: Int -> ConRep
  -- Constant    :: Int -> ConRep
  -- Transparent :: ConRep
  -- Transu      :: ConRep
  -- Transb      :: ConRep
  -- Ref         :: ConRep
  -- Variable    :: Var -> AccessPath -> ConRep
  -- VariableC   :: Var -> AccessPath
  deriving (Eq, Show)

{-
data Con where
  DataCon   :: ConRep -> Con
  IntCon    :: Int -> Con
  RealCon   :: T.Text -> Con
  StringCon :: T.Text -> Con
  deriving (Eq, Show)
-}

data PrimOp where
  -- arithmetic expressions
  Add  :: LExp -> LExp -> PrimOp
  Sub  :: LExp -> LExp -> PrimOp
  Mul  :: LExp -> LExp -> PrimOp
  Div  :: LExp -> LExp -> PrimOp

  -- boolean expressions
  Eql  :: LExp -> LExp -> PrimOp
  Lt   :: LExp -> LExp -> PrimOp
  And  :: LExp -> LExp -> PrimOp
  Or   :: LExp -> LExp -> PrimOp
  Not  :: LExp -> PrimOp

  -- other
  Cond :: LExp -> LExp -> LExp -> PrimOp
  deriving (Eq, Show)

-- expressions
data LExp where
  ILit   :: Int -> LExp 
  BLit   :: Bool -> LExp 
  RLit   :: Double -> LExp 
  TLit   :: T.Text -> LExp 
  Fix    :: [(VarName, LExp)] -> LExp -> LExp 
  Fun    :: VarName -> LExp -> LExp 
  Var    :: VarName -> LExp
  App    :: LExp -> LExp -> LExp 
  Switch :: LExp -> [ConRep] -> [(ConRep, LExp)] -> Maybe LExp -> LExp 
  Con    :: ConRep -> LExp -> LExp 
  Decon  :: ConRep -> LExp -> LExp 
  Record :: [LExp] -> LExp 
  Select :: Int -> LExp -> LExp 
  -- Raise  :: LExp -> LExp 
  -- Handle :: LExp -> LExp -> LExp 
  Prim   :: PrimOp -> LExp 
  deriving (Eq, Show)
  
