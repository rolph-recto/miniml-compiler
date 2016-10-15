import qualified Data.Map.Strict as M

import MiniML.AST
import MiniML.CPS
-- import MiniML.Interp

main = do
  putStrLn "Hello world"
  {-
  let fac = LFun 1 (LPrim
              (LCond (LPrim (LEql (LVar 1) (LInt 1)))
                    (LInt 1)
                    (LPrim (LMul (LVar 1) (LApp (LVar 0) (LPrim (LSub (LVar 1) (LInt 1))))))))
  -}
  let sqr     = LFun 1 (LApp (LPrim LMul) (LRecord [LVar 1, LVar 1]))
  let prog n  = LFix [(0, sqr)] (LApp (LVar 0) (LInt n))
  let prog2 n = LApp sqr (LInt n)
  let res = runToCPS (prog2 5)
  case res of 
    Left err -> print err
    Right rval -> print rval
