{-

This is a test suite for the SECD instruction functions in the CESMachine module

-}

module CESTests where

import Control.Monad.State.Lazy
import Compiler
import CESMachine

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