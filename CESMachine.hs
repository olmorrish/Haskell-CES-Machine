module CESMachine where

--import DeBruijn as DB   --so we can access our lambda datatype
import Control.Monad.State.Lazy
import Data.List

data StackElement =
    Clos ([String],[String])  --closure case
  | Val Int
  | MTrue
  | MFalse
  | Nil
  | Cons (StackElement,StackElement)
  deriving (Eq, Show)

--data CodeElement = String | CodeIf (String, String)

type CES = ([String] , [String] , [StackElement])


--------------------------------------------------------------------------
---- Running the CES
--------------------------------------------------------------------------

runMachine :: [String] -> State CES Int
runMachine code = do
  initializeMachine code      --load up the machine and run it until complete
  getFixedPoint
  finalCES <- get             --grab the answer from the top of the stack
  let ans = getAns finalCES
  return 0

--recursively call this function on the top instruction until a final state is reached
getFixedPoint :: State CES Int
getFixedPoint = do
  step
  currentState <- get
  if inFinalState currentState
    then return 0
  else getFixedPoint

step :: State CES Int
step = do
  state <- get
  let (cTop:c, _, _) = state
  case cTop of
    "Nil" -> do
      instNil
      return 0
--TODO WRITE ALL CASES OF INSTRUCTIONS

--takes the initial instruction set and begins the machine
initializeMachine :: [String] -> State CES Int
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
  put (c, env, Clos (c, env) : st)
  return 0

instApplication :: State CES Int
instApplication = do
  state <- get
  let (c, env, Clos (c', env'):Val v:st) = state
  put (c', show v : env', Clos (c, env) : st)
  return 0

instAccess :: Int -> State CES Int
instAccess n = do
  state <- get
  let (c, env, st) = state
  let v = read (env!!n) :: Int -- get the nth element of env and turn it from a String to an Int
  put (c, env, Val v:st)       --  then wrap that Int in a Val type constructor
  return 0

instRet :: State CES Int
instRet = do
  state <- get
  let (c, env, v:Clos(c',env'):st) = state
  put (c', env', v:st)
  return 0

---------------------------------------
---- Arithmetic Instructions
---------------------------------------
instConst :: Int -> State CES Int
instConst k = do
  state <- get
  let (c, env, st) = state
  put (c, env, Val k : st) --show converts k from Int to String
  return 0

instAdd :: State CES Int
instAdd = do
  state <- get
  let (c, env, Val s1 : Val s2 : st) = state
  put (c, env, Val(s1+s2):st)  -- both top items are combined
  return 0

instMul :: State CES Int
instMul = do
  state <- get
  let (c, env, Val s1 : Val s2 : st) = state
  put (c, env, Val (s1*s2):st)   --same as above
  return 0

instLeq :: State CES Int
instLeq = do
  state <- get
  let (c, env, Val s1:Val s2:st) = state
  if s1 <= s2
    then put (c, env, MTrue : st)
    else put (c, env, MFalse : st)
  return 0

---------------------------------------
---- Boolean Instructions
---------------------------------------
instTrue :: State CES Int
instTrue = do
  state <- get
  let (c, env, st) = state
  put (c, env, MTrue:st)
  return 0

instFalse :: State CES Int
instFalse = do
  state <- get
  let (c, env, st) = state
  put (c, env, MFalse:st)
  return 0

--the actual snippets of code are passed in as arguments for simplicity; parsing was getting weird otherwise
instIf :: ([String], [String]) -> State CES Int
instIf (c0,c1) = do
  state <- get
  let (c, env, toEval:st) = state
  if toEval == MTrue then do
    put (c0, env, Clos(c,env):st)
    return 0
  else if toEval == MFalse then do
    put (c1, env, Clos(c,env):st)
    return 0
  else return (-1)

---------------------------------------
---- List Instructions
---------------------------------------

instNil :: State CES Int
instNil = do
  state <- get
  let (c, env, st) = state
  put (c, env, Nil:st)
  return 0

instCons :: State CES Int
instCons = do
  state <- get
  let (c, env, v1:v2:st) = state
  put (c, env, Cons(v1,v2):st)
  return 0

--TODO pattern matching nonsense prevented me from having a fail-case
instCase :: ([String], [String]) -> State CES Int
instCase (c1,c2) = do
  state <- get
  let (c, env, toEval:st) = state
  if toEval == Nil then do
    put (c2, env, Clos(c,env):st)
    return 0
  else do
    let Cons(v1,v2) = toEval
    put (c1, show v1:show v2:env, Clos(c,env):st) --TODO ensure show works here; going from StackElement -> String
    return 0

---------------------------------------
---- Support Functions
---------------------------------------

-- Ex: "(aaaa, bbbb)" --> ("aaaa", "bbbb")
readPair :: String -> (String, String)
readPair s =
  let i = elemIndex ',' s in
   case i of
     Just i    -> (take (i-1) (cutBrackets s) , drop i (cutBrackets s))
     otherwise -> ("","")

cutBrackets :: String -> String
cutBrackets [] = []
cutBrackets [x] = []
cutBrackets xs = init (tail xs)
