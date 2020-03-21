{-

The CESMachine module contains functions to run a CES machine, given a
list of SECD instructions from the Compiler module.

-}

module CESMachine where

import Control.Monad.State.Lazy
import Data.List
import Compiler

-- Datatype for information that's stored on the CES stack
-- This makes accessing and manipulating stack data much easier
data StackElement =
    MClos (Prog,[Int])  --closure case
  | MVal Int
  | MTrue | MFalse
  | MNil
  | MCons (StackElement,StackElement)
  deriving (Eq, Show)

-- State monad is run with the following type for the CES:
-- Remember that Prog := [SECDInstruction]
type CES = (Prog, [Int], [StackElement])

popInst :: CES -> CES
popInst (c:cs,env,st) = (cs,env,st)
popInst (c, env, st) = ([], env, st)

--------------------------------------------------------------------------
---- Full Process
--------------------------------------------------------------------------

solveLambdaWithCES :: Lambda -> StackElement
solveLambdaWithCES exp =
  let dbExp = translateToDeBruijn exp ([], 0)
   in evalState (runProgram (compile dbExp)) ([], [], [])

--------------------------------------------------------------------------
---- Running the CES
--------------------------------------------------------------------------

runProgram :: Prog -> State CES StackElement
runProgram code = do
  initializeMachine code      --load up the machine and run it until complete
  getFixedPoint
  finalCES <- get             --grab the answer from the top of the stack
  let ans = getAns finalCES
  return ans

--recursively call this function on the top instruction until a final state is reached
getFixedPoint :: State CES Int
getFixedPoint = do
  step
  currentState <- get
  if inFinalState currentState
    then return 0
  else getFixedPoint

-- the step function DOES NOT pop the instruction; this varies by instruction
step :: State CES Int
step = do
  state <- get
  let (inst:c, env, st) = state
  case inst of
    CLO prg -> do       -- Main Instructions
      instClosure
      return 0
    APP -> do
      instApplication
      return 0
    ACCESS n -> do
      instAccess n
      return 0
    RET -> do
      instRet
      return 0
    CONST c -> do       -- Arithmetic
      instConst c
      return 0
    ADD -> do
      instAdd
      return 0
    SUB -> do
      instSub
      return 0
    MUL -> do
      instMul
      return 0
    LEQ -> do
      instLeq
      return 0
    TRUE -> do         -- Conditionals
      instTrue
      return 0
    FALSE -> do
      instFalse
      return 0
    IF progs -> do
      instIf progs
      return 0
    CONS -> do        -- List
      instCons
      return 0
    NIL -> do
      instNil
      return 0
    CASE progs -> do
      instCase progs
      return 0

{-
data SECDInstruction =
  CLO Prog | APP | ACCESS Int | RET   -- Main Instructions
  | CONST Int | ADD | SUB | MUL | LEQ -- Arithmetic
  | TRUE | FALSE | IF (Prog, Prog)    -- Conditionals
  | CONS | NIL | CASE (Prog, Prog)    -- List
  deriving (Show, Eq)
-}

--takes the initial instruction set and begins the machine
initializeMachine :: Prog -> State CES Int
initializeMachine c = do
  put (c, [], [])
  return 0

getAns :: CES -> StackElement
getAns state =
  let (_,_, ans:st) = state in ans

inFinalState :: CES -> Bool
inFinalState state =
  let (c, env, _) = state
   in (null c && null env)






--------------------------------------------------------------------------
---- Machine State Manipulation - Instructions
--------------------------------------------------------------------------

-------------------------------------
-- Basic Instructions
-------------------------------------
instClosure :: State CES Int
instClosure = do
  state <- get
  let (c, env, st) = state
  put (c, env, MClos (c, env) : st)
  return 0

instApplication :: State CES Int
instApplication = do
  state <- get
  let (c, env, MClos (c', env'):MVal v:st) = state
  put (c', v:env', MClos (c, env) : st)
  return 0

instAccess :: Int -> State CES Int
instAccess n = do
  state <- get
  let (c, env, st) = state
  let v = (env!!n) :: Int -- get the nth element of env and turn it from a String to an Int
  put (c, env, MVal v:st)       --  then wrap that Int in a Val type constructor
  return 0

instRet :: State CES Int
instRet = do
  state <- get
  let (c, env, v:MClos(c',env'):st) = state
  put (c', env', v:st)
  return 0

---------------------------------------
---- Arithmetic Instructions
---------------------------------------
instConst :: Int -> State CES Int
instConst k = do
  state <- get
  let (c, env, st) = state
  put (popInst (c, env, MVal k : st)) --show converts k from Int to String
  return 0

instAdd :: State CES Int
instAdd = do
  state <- get
  let (c, env, MVal s1 : MVal s2 : st) = state
  put (popInst (c, env, MVal(s1+s2):st))  -- both top items are combined
  return 0

instSub :: State CES Int
instSub = do
  state <- get
  let (c, env, MVal s1 : MVal s2 : st) = state
  put (popInst (c, env, MVal(s1-s2):st))
  return 0

instMul :: State CES Int
instMul = do
  state <- get
  let (c, env, MVal s1 : MVal s2 : st) = state
  put (popInst (c, env, MVal (s1*s2):st))   --same as above
  return 0

instLeq :: State CES Int
instLeq = do
  state <- get
  let (c, env, MVal s1:MVal s2:st) = state
  if s1 <= s2
    then put (popInst (c, env, MTrue : st))
    else put (popInst (c, env, MFalse : st))
  return 0

---------------------------------------
---- Boolean Instructions
---------------------------------------
instTrue :: State CES Int
instTrue = do
  state <- get
  let (c, env, st) = state
  put (popInst (c, env, MTrue:st))
  return 0

instFalse :: State CES Int
instFalse = do
  state <- get
  let (c, env, st) = state
  put (popInst (c, env, MFalse:st))
  return 0

--the actual snippets of code are passed in as arguments for simplicity; parsing was getting weird otherwise
instIf :: (Prog, Prog) -> State CES Int
instIf (c0,c1) = do
  state <- get
  let (c:cs, env, toEval:st) = state
  if toEval == MTrue then do
    put (c0, env, MClos(cs,env):st)
    return 0
  else if toEval == MFalse then do
    put (c1, env, MClos(cs,env):st)
    return 0
  else return (-1)

---------------------------------------
---- List Instructions
---------------------------------------

instNil :: State CES Int
instNil = do
  state <- get
  let (c, env, st) = state
  put (popInst (c, env, MNil:st))
  return 0

instCons :: State CES Int
instCons = do
  state <- get
  let (c, env, v1:v2:st) = state
  put (popInst (c, env, MCons(v1,v2):st))
  return 0

--TODO pattern matching nonsense prevented me from having a fail-case
instCase :: (Prog, Prog) -> State CES Int
instCase (c1,c2) = do
  state <- get
  let (c:cs, env, toEval:st) = state
  if toEval == MNil then do
    put (c2, env, MClos(cs,env):st)
    return 0
  else do
    let MCons(MVal v1, MVal v2) = toEval
    put (c1, v1:v2:env, MClos(cs,env):st) --TODO ensure show works here; going from StackElement -> String
    return 0

---------------------------------------
---- Support Functions
---------------------------------------

---- Ex: "(aaaa, bbbb)" --> ("aaaa", "bbbb")
--readPair :: String -> (String, String)
--readPair s =
--  let i = elemIndex ',' s in
--   case i of
--     Just i    -> (take (i-1) (cutBrackets s) , drop i (cutBrackets s))
--     otherwise -> ("","")
--
--cutBrackets :: String -> String
--cutBrackets [] = []
--cutBrackets [x] = []
--cutBrackets xs = init (tail xs)
