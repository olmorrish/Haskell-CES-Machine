{-

The Compiler module contains functions for the following:
  1. Translating extended Lambda expressions into DeBruijn notation
  2. Compiling DeBruijn Lambda expressions into a list of SECD instructions

-}

module Compiler where

--------------------------------------------------------------------------
---- Lambda Data and DeBruijn Conversion
--------------------------------------------------------------------------

-- data type for regular lambda expressions
data Lambda
  = Var String
  | App Lambda Lambda
  | Abs String Lambda         -- basic Lambda Term
  | Add Lambda Lambda
  | Sub Lambda Lambda
  | Mul Lambda Lambda
  | Leq Lambda Lambda
  | Num Int                   -- arithmetic term
  | Cond Lambda Lambda Lambda -- "if" picks between 2 & 3 based on truth value of first Lambda
  | LTrue                     --represents a \xy.x or \xy.y
  | LFalse
  | Nil
  | Cons Lambda Lambda
  | Case Lambda Lambda Lambda -- picks between 2 & 3 based on if the first Lambda is Nil or Cons
  | Fix Lambda
  deriving (Show, Eq)

-- switching to debruijn notation
-- \x.\y.xy -> \.\2.1

--data type for DeBruijn-Notation lambda expressions
data DLambda
  = DVar Int                   --vars are now NUMBERED and are not strings
  | DApp DLambda DLambda
  | DAbs DLambda               --abstractions no longer have any variable names
  | DAdd DLambda DLambda
  | DSub DLambda DLambda
  | DMul DLambda DLambda
  | DLeq DLambda DLambda
  | DNum Int
  | DCond DLambda DLambda DLambda
  | DTrue
  | DFalse
  | DNil
  | DCons DLambda DLambda
  | DCase DLambda DLambda DLambda -- picks between 2 & 3 based on if the first Lambda is Nil or Cons
  | DFix DLambda
  deriving (Show, Eq)


type DBStack = ([(String, Int)], Int)


-- we keep a stack of variables and their assigned numbers
-- when we search for a variable, we always name it the first instance we find
--  in the stack
-- when an abstraction is entered, we increase the number
translateToDeBruijn :: Lambda -> DBStack -> DLambda
translateToDeBruijn (Var x) (ps,c)     = DVar (c - stackLookup x (ps, c))
translateToDeBruijn (App t1 t2) st     = DApp (translateToDeBruijn t1 st) (translateToDeBruijn t2 st)
translateToDeBruijn (Abs x t) (ps,c)   = DAbs (translateToDeBruijn t (push (x,c) (ps,c+1))) --push var to stack and increment counter IN THIS RECURSIVE CALL
translateToDeBruijn (Add t1 t2) st     = DAdd (translateToDeBruijn t1 st) (translateToDeBruijn t2 st)
translateToDeBruijn (Sub t1 t2) st     = DSub (translateToDeBruijn t1 st) (translateToDeBruijn t2 st)
translateToDeBruijn (Mul t1 t2) st     = DMul (translateToDeBruijn t1 st) (translateToDeBruijn t2 st)
translateToDeBruijn (Leq t1 t2) st     = DLeq (translateToDeBruijn t1 st) (translateToDeBruijn t2 st)
translateToDeBruijn (Num n) st         = DNum n
translateToDeBruijn (Cond t1 t2 t3) st = DCond (translateToDeBruijn t1 st) (translateToDeBruijn t2 st) (translateToDeBruijn t3 st)
translateToDeBruijn LTrue st           = DTrue
translateToDeBruijn LFalse st          = DFalse
translateToDeBruijn Nil st             = DNil
translateToDeBruijn (Cons t1 t2) st    = DCons (translateToDeBruijn t1 st) (translateToDeBruijn t2 st)
translateToDeBruijn (Case t1 t2 t3) st = DCase (translateToDeBruijn t1 st) (translateToDeBruijn t2 st) (translateToDeBruijn t3 st)
translateToDeBruijn (Fix t) st         = DFix (translateToDeBruijn t st)

stackLookup :: String -> DBStack -> Int
stackLookup x ([], c) = -1  -- TODO FIGURE THIS OUT; IS A BUG -> What is db notation when a var has no binding?
stackLookup x ((y,n):ps, c) =
  if x==y then n
  else stackLookup x (ps,c)

push :: (String, Int) -> DBStack -> DBStack
push p (ps,c) = (p:ps,c)

--------------------------------------------------------------------------
---- DeBruijn Compilation to CES Instructions
--------------------------------------------------------------------------

-------------------------------------
-- Data Types and Manipulations
-------------------------------------

type Prog = [SECDInstruction]

data SECDInstruction =
  CLO Prog | APP | ACCESS Int | RET   -- Main Instructions
  | CONST Int | ADD | SUB | MUL | LEQ -- Arithmetic
  | TRUE | FALSE | IF (Prog, Prog)    -- Conditionals
  | CONS | NIL | CASE (Prog, Prog)    -- List
  | FIX Prog | FIXC Prog              -- Fixed Point
  deriving (Show, Eq)

--type VariableContext = [Int]

-------------------------------------
-- Compilation
-------------------------------------

--according to page 16
compile :: DLambda -> Prog
compile (DAbs t)         = [CLO(compile t ++ [RET])]
compile (DApp m n)       = compile n ++ compile m ++ [APP]
compile (DVar x)         = [ACCESS x]                         --some steps left out; presumably DB notation simplifies the procedure
compile (DAdd x y)       = compile y ++ compile x ++ [ADD]
compile (DSub x y)       = compile y ++ compile x ++ [SUB]   -- SUB is not in the notes, but is simple so was still included
compile (DMul x y)       = compile y ++ compile x ++ [MUL]
compile (DLeq x y)       = compile y ++ compile x ++ [LEQ]
compile (DNum n)         = [CONST n]
compile DTrue            = [TRUE]
compile DFalse           = [FALSE]
compile (DCond t x y)    = compile t ++ [IF(compile x ++ [RET], compile y ++ [RET])]     --IF and CASE both carry two alt program paths as a (Prog, Prog) tuple
compile DNil             = [NIL]
compile (DCons x y)      = compile y ++ compile x ++ [CONS]
compile (DCase t x y)    = compile t ++ [CASE(compile x ++ [RET], compile y ++ [RET])]
compile (DFix t)  = case t of
  DAbs (DAbs (DAbs n)) -> [FIX(compile n ++ [RET])] --TODO how can this work when variables are anonymous?
  DAbs (DAbs n)        -> [FIXC(compile n ++ [RET])]