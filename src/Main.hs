import qualified Data.Map.Strict as M

import MiniML.AST
import MiniML.Interp

main = do
  let prog = Fix [(0, Fun 1 (Prim (Add (Var 1) (Var 1))))] (App (Var 0) (ILit 99))
  res <- runMiniML prog
  case res of 
    Left err -> print err
    Right rval -> print rval
