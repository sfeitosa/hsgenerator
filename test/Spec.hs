{-# LANGUAGE TemplateHaskell #-}
-- QuickCheck property-based testing for Haskell
-- Author: <Anonymous>
-- Date: 06/2024
-----------------------------------------------------------
module Main where 

import HSGenModule
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad
import System.Process
import System.Exit

-- Function: prop_generate
-- Objective: Generates and writes a number of programs on the disk.
-- Params: None.
-- Returns: True
----------------------------------------------------------------------
prop_generate = 
  forAll genModule $
    \m -> monadicIO $ do f <- run (writeFile "program.hs" (show m))
                         assert True

-- Function: prop_compile
-- Objective: Tests if the generated programs are compiled by 'ghc'.
-- Params: None.
-- Returns: True if all programs compile, False otherwise.
----------------------------------------------------------------------
-- prop_compile = 
--   forAll genModule $
--     \m -> monadicIO $ do f <- run (writeFile "program.hs" (show m))
--                          (ex,out,err) <- run (readProcessWithExitCode "ghc" ["-w", "program.hs"] "")
--                          assert (ex == ExitSuccess)

-- Function: main
-- Objective: Run all properties using QuickCheck.
--------------------------------------------------
return [] -- This is necessary for using 'forAllProperties' function
main = $forAllProperties $
          quickCheckWithResult (stdArgs { maxSuccess = 1000 })