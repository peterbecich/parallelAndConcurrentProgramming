module Lib
    ( someFunc
    ) where

import Chapter2EvalMonad.EvalMonad
import Chapter2EvalMonad.Forcing
import Chapter2EvalMonad.Lazy
import Chapter2EvalMonad.Sudoku
import Chapter3EvaluationStrategies.Strategies
import Chapter6Accelerate.DotProduct
import Chapter6Accelerate.Mandelbrot
import Chapter6Accelerate.ShortestPaths
import Chapter7BasicConcurrency.Beep
import Chapter7BasicConcurrency.Fork
import Chapter7BasicConcurrency.MVars
import Chapter10STM.Notes
import Chapter12ConcurrentNetworkServers.TrivialServer

someFunc :: IO ()
someFunc = putStrLn "someFunc"
