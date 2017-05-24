module Chapter6Accelerate.Mandelbrot where

import qualified Prelude as P
import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.PTX     as GPU

-- http://chimera.labs.oreilly.com/books/1230000000929/ch06.html#sec_par-accel-cuda-debugging

type F = Float
type Complex = (F, F)  -- tuples allowed
type ComplexPlane = Array DIM2 Complex

next :: Exp Complex -> Exp Complex -> Exp Complex
next c z = c `plus` (z `times` z)

-- A.fst :: (Elt b, Elt a) => Exp (a, b) -> Exp a
-- A.snd :: (Elt b, Elt a) => Exp (a, b) -> Exp b
-- A.lift :: Lift c e => e -> c (Plain e)
plus :: Exp Complex -> Exp Complex -> Exp Complex
plus c1 c2 = let
  r1 = fst c1
  i1 = snd c1
  r2 = fst c2
  i2 = snd c2
  in lift (r1+r2, i1+i2)

times :: Exp Complex -> Exp Complex -> Exp Complex
times c1 c2 = undefined

-- (?) :: Elt t => Exp Bool -> (Exp t, Exp t) -> Exp t  
