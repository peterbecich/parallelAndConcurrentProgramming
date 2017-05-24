module Chapter6Accelerate.DotProduct where

-- http://www.acceleratehs.org/get-started.html

import Data.Array.Accelerate              as A
-- import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU

dotp :: Acc (A.Vector Float) -> Acc (A.Vector Float) -> Acc (A.Scalar Float)
dotp xs ys = fold (+) 0 (A.zipWith (*) xs ys)

xs = fromList (Z:.10) [0..]   :: A.Vector Float
ys = fromList (Z:.10) [1,3..] :: A.Vector Float

dp = GPU.run $ dotp (use xs) (use ys)

-- http://www.acceleratehs.org/documentation/users-guide/language.html

-- A.Vector :: * -> *

-- fromList :: (Elt e, Shape t) => t -> [e] -> Array t e
-- ten :: A.Vector DIM1 Int
ten :: A.Vector Int
ten = fromList (Z :. 10) [1..10]

-- A.Array :: * -> * -> *

fifteen :: A.Array A.DIM2 Int
fifteen = fromList (Z :. 3 :. 5) [1..15]

fifteen' :: A.Array A.DIM2 Int
fifteen' = fromList (Z :. 3 :. 5) [1..]

thirteen :: Int
thirteen = indexArray fifteen' (Z :. 2 :. 2)

-- run :: Arrays a => Acc a -> a
-- GPU.run :: Arrays a => Acc a -> a
-- use :: Arrays arrays => arrays -> Acc arrays
-- A.map
--   :: (Elt b, Elt a, Shape t) =>
--      (Exp a -> Exp b) -> Acc (Array t a) -> Acc (Array t b)

sixteen = GPU.run $ A.map (+1) (use fifteen')

-- Matrix (Z :. 3 :. 5) 
--   [ 2, 3, 4, 5, 6,
--     7, 8, 9,10,11,
--    12,13,14,15,16]

squares = GPU.run $ A.map (Prelude.^ 2) (use fifteen)


-- https://hackage.haskell.org/package/accelerate-1.0.0.0/docs/Data-Array-Accelerate.html#t:Elt

-- unit :: Elt e => Exp e -> Acc (Scalar e)

three = GPU.run $ unit (3 :: Exp Int)

-- three' = the $ unit (3 :: Exp Int)

