{-

This is a test suite for the SECD instruction functions in the CESMachine module

-}

module CESTests where

import Control.Monad.State.Lazy
import Compiler
import CESMachine

runAllInstructionTests :: IO ()
runAllInstructionTests =
  let ret = (runAllMainTests ++ runAllArithmeticTests ++ runAllBoolTests ++ runAllListTests)
    in putStrLn ret

-------------------------------------
-- Basic Instruction Tests
-------------------------------------

runAllMainTests :: String
runAllMainTests = unlines ([""] ++ [">> MAIN TESTS <<"] ++ [testInstClosure_1] ++ [testInstClosure_2])

testInstClosure_1 :: String
testInstClosure_1 = "undefined" -- TODO
--let x = execState instClosure ([NIL],["abc"],[]) in
--  if x == ([NIL],[],[])

testInstClosure_2 :: String
testInstClosure_2 = "undefined" --TODO

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
---- Arithmetic Instruction Tests
---------------------------------------

runAllArithmeticTests :: String
runAllArithmeticTests = unlines ([""] ++ [">> ARITHMETIC TESTS <<"] ++ [testInstConst_1] ++ [testInstConst_2]
  ++ [testInstAdd] ++ [testInstSub] ++ [testInstMul] ++ [testInstLeq_1] ++ [testInstLeq_2]
  ++ [testAddMul])

testInstConst_1 :: String
testInstConst_1 = let x = execState (instConst 3) ([],[],[]) in
  if x==([],[],[MVal 3])
    then "CONST 1 PASS " ++ show x
  else "CONST 1 FAIL " ++ show x

testInstConst_2 :: String
testInstConst_2 = let x = execState step ([CONST 3],[],[]) in
  if x==([],[],[MVal 3])
    then "CONST 1 PASS " ++ show x
  else "CONST 1 FAIL " ++ show x

testInstAdd :: String
testInstAdd = let x = execState step ([ADD],[],[MVal 3, MVal 4]) in
  if x==([],[],[MVal 7])
    then "ADD PASS " ++ show x
  else "ADD FAIL " ++ show x

testInstSub :: String
testInstSub = let x = execState step ([SUB],[],[MVal 3, MVal 4]) in
  if x==([],[],[MVal (-1)])
    then "SUB PASS " ++ show x
  else "SUB FAIL " ++ show x

testInstMul :: String
testInstMul = let x = execState step ([MUL],[],[MVal 3, MVal 4]) in
  if x==([],[],[MVal 12])
    then "MUL PASS " ++ show x
  else "MUL FAIL " ++ show x

testInstLeq_1 :: String
testInstLeq_1 = let x = execState step ([LEQ],[],[MVal 3, MVal 4]) in
  if x==([],[],[MTrue])
    then "LEQ 1 PASS " ++ show x
  else "LEQ 1 FAIL " ++ show x

testInstLeq_2 :: String
testInstLeq_2 = let x = execState step ([LEQ],[],[MVal 1, MVal 1]) in
  if x==([],[],[MTrue])
    then "LEQ 2 PASS " ++ show x
  else "LEQ 2 FAIL " ++ show x

testAddMul :: String
testAddMul = let x = execState step (execState step ([ADD, MUL],[],[MVal 2, MVal 3, MVal (-4)])) in
  if x==([],[],[MVal (-20)])
    then "ADDMUL PASS " ++ show x
  else "ADDMUL FAIL " ++ show x

---------------------------------------
---- Boolean Instructions
---------------------------------------

runAllBoolTests :: String
runAllBoolTests = unlines ([""] ++ [">> BOOLEAN TESTS <<"] ++ [testInstTrue_1] ++ [testInstTrue_2]
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
  if x==([NIL],[],[MClos([],[])])
    then "IF 1 PASS " ++ show x
  else "IF 1 FAIL " ++ show x

testInstIf_2 :: String
testInstIf_2 = let x = execState step ([IF([CONST 1],[CONST 2])],[0],[MTrue]) in
  if x==([CONST 1],[0],[MClos([],[0])])
    then "IF 2 PASS " ++ show x
  else "IF 2 FAIL " ++ show x

testInstIf_3 :: String
testInstIf_3 = let x = execState step ([IF([CONST 1],[CONST 2])],[0],[MFalse]) in
  if x==([CONST 2],[0],[MClos([],[0])])
    then "IF 3 PASS " ++ show x
  else "IF 3 FAIL " ++ show x

---------------------------------------
---- List Instruction Tests
---------------------------------------

runAllListTests :: String
runAllListTests = unlines ([""] ++ [">> LIST TESTS <<"] ++ [testInstNil_1] ++ [testInstNil_2]
  ++ [testInstCons_1] ++ [testInstCons_2] ++ [testInstCase_1])

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

testInstCons_1 :: String
testInstCons_1 = let x = execState step ([CONS],[],[MVal 1, MVal 2]) in
  if x==([],[],[MCons(MVal 1, MVal 2)])
    then "CONS 1 PASS " ++ show x
  else "CONS 1 FAIL " ++ show x

testInstCons_2 :: String
testInstCons_2 = let x = execState step ([CONS],[],[MNil, MFalse]) in
  if x==([],[],[MCons(MNil, MFalse)])
    then "CONS 2 PASS " ++ show x
  else "CONS 2 FAIL " ++ show x

testInstCase_1 :: String
testInstCase_1 = let x = execState step ([CASE([CONST 1], [CONST 2])],[],[MCons(MVal 3, MVal 4)]) in
  if x==([CONST 1],[3,4],[MClos([],[])])
    then "CASE 1 PASS " ++ show x
  else "CASE 1 FAIL " ++ show x