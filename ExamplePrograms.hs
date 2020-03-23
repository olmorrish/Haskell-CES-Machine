{-

The ExamplePrograms module contains five functions that pass a Lambda datatype into
  the "solveLambdaWithCES" function, which translates, compiles, and runs it.

****For a quick demonstration of all examples, please run the "runExamplePrograms" function.****

  Note: as of this submission, the fifth (absolute value) function
    is not working due to an instruction function bug.
    
-}

module ExamplePrograms where

import Compiler
import CESMachine

-------------------------------------
-- Program Runtime Tests
-------------------------------------

runExamplePrograms :: IO ()
runExamplePrograms = putStrLn (unlines ([""]
  ++ ["Question         Final StackElement"]
  ++ ["----------       ------------------"]
  ++ ["Succ 2?    ----> " ++ succ' 2]
  ++ ["Succ (-1)? ----> " ++ succ' (-1)]
  ++ ["2>3?       ----> " ++ greaterThan 2 3]
  ++ ["(-7)>5?    ----> " ++ greaterThan (-7) 5]
  ++ ["12:5?      ----> " ++ concInts 12 5]
  ++ ["(-4):7?    ----> " ++ concInts (-4) 7]
  ++ ["(-3) * 7?  ----> " ++ product' (-3) 7]
  ++ ["(-12) * (-8)? -> " ++ product' (-12) (-8)]))

-- Successor Function
-- Demonstrates usage of arithmetic components of the CES
-- (\x.x+1)
succ' :: Int -> String
succ' input = show (solveLambdaWithCES (succLambda (Num input)))
  where succLambda = App (Abs "x" (Add (Var "x") (Num 1)))

-- Greater Than Function
-- Demonstrates usage of conditional component of the CES
-- (\xy.Cond (Leq x y) LFalse LTrue)
greaterThan :: Int -> Int -> String
greaterThan v1 v2 = let lambda = App (Abs "x" (App (Abs "y" gThan) (Num v2)))   (Num v1)
  in show (solveLambdaWithCES lambda)
  where gThan = Cond (Leq (Var "x") (Var "y")) LFalse LTrue

-- Concatenation of Integers Function
-- Demonstrates usage of Cons component of the CES
-- (\xy.(Cons x y))
concInts :: Int -> Int -> String
concInts v1 v2 = let lambda = App (Abs "x" (App (Abs "y" cons) (Num v2)))    (Num v1)
  in show (solveLambdaWithCES lambda)
  where cons = Cons (Var "x") (Var "y")

-- Product Function
-- Demonstrates usage of arithmetic components of the CES
-- (\xy.(Mul x y))
product' :: Int -> Int -> String
product' v1 v2 = let lambda = App (Abs "x" (App (Abs "y" prod) (Num v2)))   (Num v1)
  in show (solveLambdaWithCES lambda)
  where prod = Mul (Var "x") (Var "y")

--
--abs' :: Int -> String
--abs' n =
--  let exp = App (Abs "x" (Cond (isPositive (Var "x")) (Var "x") (App addSelf (Var "x")))) (Num n)
--   in show (solveLambdaWithCES exp)
--  where
--    addSelf = Add (Var "x") (Var "x")
--    isPositive = Cond (Leq (-1) (Var "x")) LFalse LTrue
