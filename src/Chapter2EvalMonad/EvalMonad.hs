module Chapter2EvalMonad.EvalMonad where

-- https://hackage.haskell.org/package/parallel-3.2.1.1/docs/Control-Parallel-Strategies.html

import Control.Parallel.Strategies

-- x = sum [1..1000]
-- y = sum [1..10000]
-- z = sum [1..100000]
-- w = x + sum [20..200]

summer :: Integer -> Integer
summer x = sum [1..x]

w = summer 100000
x = summer 1000000
y = summer 10000000
z = w + x + y

w' = summer 100000
x' = summer 1000000
y' = summer 10000000
z' = w + x + y

x1 = summer 1000000
x2 = summer 1000000
x3 = summer 1000000
x4 = summer 1000000

x1' = summer 1000000
x2' = summer 1000000
x3' = summer 1000000
x4' = summer 1000000

-- :t runEval
-- runEval :: Eval a -> a
-- :t rpar
-- rpar :: Strategy a
-- :t rseq
-- rseq :: Strategy a

foo = do
  wsum <- rpar w
  xsum <- rpar x
  ysum <- rpar y
  zsum <- rpar z
  return (wsum, xsum, ysum, zsum)
  
foo' = runEval foo


bar = do
  wsum <- rseq w'
  xsum <- rseq x'
  ysum <- rseq y'
  zsum <- rseq z'
  return (wsum, xsum, ysum, zsum)
  
bar' = runEval foo

foox = do
  xsum1 <- rpar x1
  xsum2 <- rpar x2
  xsum3 <- rpar x3
  xsum4 <- rpar x4
  return (xsum1, xsum2, xsum3, xsum4)

foox' = runEval foox

barx = do
  xsum1 <- rseq x1'
  xsum2 <- rseq x2'
  xsum3 <- rseq x3'
  xsum4 <- rseq x4'
  return (xsum1, xsum2, xsum3, xsum4)

barx' = runEval barx

-- refresh repl after every test, due to memoization

foox1 = do
  xsum1 <- rpar x1
  xsum2 <- rseq x2
  return (xsum1, xsum2)

foox1' = runEval foox1

foox2 = do
  xsum1 <- rpar x1
  xsum2 <- rseq x2
  _ <- rseq xsum1
  return (xsum1, xsum2)

foox2' = runEval foox2

