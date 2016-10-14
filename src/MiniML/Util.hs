module MiniML.Util (
 Var, AccessPath(..)
) where

type Var = Int

data AccessPath = OFFp Int | SELp Int AccessPath deriving (Show)



