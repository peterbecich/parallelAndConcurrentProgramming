module Chapter2EvalMonad.Forcing where

import Control.DeepSeq

-- force :: NFData a => a -> a
-- rnf :: NFData a => a -> ()

data Tree a = Empty | Branch (Tree a) a (Tree a) deriving (Show) -- show may evaluate, defeating the demonstration

fooTree = Branch (Branch Empty 1 Empty) 2 (Branch Empty 3 (Branch Empty 5 Empty))

instance NFData a => NFData (Tree a) where
  rnf Empty = () -- "effect" of evaluating
  rnf (Branch l x r) = rnf l `seq` rnf x `seq` rnf r -- force post-order traversal

  
