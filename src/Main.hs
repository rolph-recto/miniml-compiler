import qualified Data.Map.Strict as M

import MiniML.AST
import MiniML.Interp

main = do
  let fac = Fun 1 (Prim
              (Cond (Prim (Eql (Var 1) (ILit 1)))
                    (ILit 1)
                    (Prim (Mul (Var 1) (App (Var 0) (Prim (Sub (Var 1) (ILit 1))))))))
  let sqr  = Fun 1 (Prim (Mul (Var 1) (Var 1)))
  let prog n = Fix [(0, fac)] (App (Var 0) (ILit n))
  res <- runMiniML (prog 5)
  case res of 
    Left err -> print err
    Right rval -> print rval
