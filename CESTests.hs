{-

This is a test suite for the SECD instruction functions in the CESMachine module

-}

module CESTests where

import Control.Monad.State.Lazy
import Compiler
import CESMachine

runAllInstructionTests :: IO ()
runAllInstructionTests =
  putStrLn (runAllBoolTests)

-------------------------------------
-- Basic Instructions
-------------------------------------

--testInstClosure_1 :: CES
--testInstClosure_1 = let x = execState instClosure ([NIL],["abc"],[]) in
--  if x == ([NIL],[],[])

testInstClosure_2 :: String
testInstClosure_2 = undefined --TODO

testInstApp_1 :: String
testInstApp_1 = undefined --TODO

testInstApp_2 :: String
testInstApp_2 = undefined --TODO

testInstAccess_1 :: String
testInstAccess_1 = undefined --TODO

testInstAccess_2 :: String
testInstAccess_2 = undefined --TODO

testInstRet_1 :: String
testInstRet_1 = undefined --TODO

testInstRet_2 :: String
testInstRet_2 = undefined --TODO

---------------------------------------
---- Arithmetic Instructions
---------------------------------------

testInstConst_1 :: String
testInstConst_1 = undefined --TODO

testInstConst_2 :: String
testInstConst_2 = undefined --TODO

---------------------------------------
---- Boolean Instructions
---------------------------------------

runAllBoolTests :: String
runAllBoolTests = unlines ([">> BOOLTESTS <<"] ++ [testInstTrue_1] ++ [testInstTrue_2]
  ++ [testInstFalse_1] ++ [testInstFalse_2]
  ++ [testInstIf_1] ++ [testInstIf_2] ++ [testInstIf_3])

testInstTrue_1 :: String
testInstTrue_1 = let x = execState instTrue ([],[],[]) in
  if x==([],[],[MTrue])
    then "TRUE 1 PASS " ++ show x
  else "TRUE 1 FAIL " ++ show x

testInstTrue_2 :: String
testInstTrue_2 = let x = execState step ([TRUE],[],[]) in
  if x==([],[],[MTrue])
    then "TRUE 2 PASS " ++ show x
  else "TRUE 2 FAIL " ++ show x

testInstFalse_1 :: String
testInstFalse_1 = let x = execState instFalse ([],[],[]) in
  if x==([],[],[MFalse])
    then "FALSE 1 PASS " ++ show x
  else "FALSE 1 FAIL " ++ show x

testInstFalse_2 :: String
testInstFalse_2 = let x = execState step ([FALSE],[],[]) in
  if x==([],[],[MFalse])
    then "FALSE 2 PASS " ++ show x
  else "FALSE 2 FAIL " ++ show x

testInstIf_1 :: String
testInstIf_1 = let x = execState (instIf ([NIL],[])) ([CONST 1],[],[MTrue]) in
  if x==([NIL],[],[MClos([CONST 1],[])])
    then "IF 1 PASS " ++ show x
  else "IF 1 FAIL " ++ show x

testInstIf_2 :: String
testInstIf_2 = let x = execState step ([IF([CONST 1],[CONST 2])],["e"],[MTrue]) in
  if x==([CONST 1],["e"],[MClos([],["e"])])
    then "IF 2 PASS " ++ show x
  else "IF 2 FAIL " ++ show x

testInstIf_3 :: String
testInstIf_3 = let x = execState step ([IF([CONST 1],[CONST 2])],["e"],[MFalse]) in
  if x==([CONST 2],["e"],[MClos([],["e"])])
    then "IF 3 PASS " ++ show x
  else "IF 3 FAIL " ++ show x

---------------------------------------
---- List Instructions
---------------------------------------

testInstNil_1 :: String
testInstNil_1 = let x = execState instNil ([],[],[]) in
  if x==([],[],[MNil])
    then "NIL 1 PASS " ++ show x
  else "NIL 1 FAIL " ++ show x

testInstNil_2 :: String
testInstNil_2 = let x = execState step ([NIL],[],[]) in
  if x==([],[],[MNil])
    then "NIL 2 PASS " ++ show x
  else "NIL 2 FAIL " ++ show x

--