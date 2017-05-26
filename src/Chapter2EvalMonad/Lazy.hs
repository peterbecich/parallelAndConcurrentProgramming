module Chapter2EvalMonad where

x = 1 + 2 :: Int

-- :sprint x
-- x = _
-- _ means unevaluated

y = 4

-- :sprint y
-- y = _

--  y
-- 4
-- :sprint y
-- y = 4

z = 5 + x

-- :sprint z
-- z = _

-- seq forces evaluation
-- seq z ()
-- ()
-- :sprint z
-- z = 8

w = (x, x)
