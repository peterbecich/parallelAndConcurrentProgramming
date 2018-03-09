-- module Chapter6Accelerate.ShortestPaths where

-- import qualified Prelude as P
-- import Data.Array.Accelerate
-- -- import Data.Array.Accelerate.LLVM.Native  as CPU
-- import Data.Array.Accelerate.LLVM.PTX     as GPU

-- type Weight = Int32
-- type Graph = Array DIM2 Weight

-- step :: Acc (Scalar Int) -> Acc Graph -> Acc Graph
-- step k g = generate (shape g) sp
--   where
--     k' = the k
--     sp :: Exp DIM2 -> Exp Weight
--     sp ix = let
--       (Z :. i :. j) = unlift ix
--       in
--         min (g ! (index2 i j)) (g ! (index2 i k') + g ! (index2 k' j))




