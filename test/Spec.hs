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
import System.IO
import System.Process
import System.Exit

-- Function: nextFileName
-- Objective: Generates the next file name to be used.
-- Params: None.
-- Returns: The next file name.
------------------------------------------------------
nextFileName :: IO String
nextFileName = do
  n <- readFile' "gc.tmp"
  let n' = show (read n + 1)
  writeFile "gc.tmp" n'
  return ("exec/program" ++ n)

-- Function: compileProgram
-- Objective: Compiles a program using 'ghc'.
-- Params: The file name and the optimization level.
-- Returns: The exit code of the compilation.
------------------------------------------------------
compileProgram :: String -> String -> IO ExitCode
compileProgram fn f = do
  let cmd = "ghc -w " ++ f ++ " -o " ++ fn ++ f ++ ".bin " ++ fn ++ ".hs"
  putStrLn ("Executing: " ++ cmd)
  system cmd

-- Function: execProgram
-- Objective: Executes a program.
-- Params: The file name and the optimization level.
-- Returns: The exit code, the output and the error.
------------------------------------------------------
execProgram :: String -> String -> IO (ExitCode, String, String)
execProgram fn f = do
  let cmd = "./" ++ fn ++ f ++ ".bin"
  putStrLn ("Executing: " ++ cmd)
  readCreateProcessWithExitCode (shell cmd) ""

-- Function: prop_generate
-- Objective: Generates and writes a number of programs on the disk.
-- Params: None.
-- Returns: Always True
----------------------------------------------------------------------
prop_generate = 
  forAll genModule $
    \m -> monadicIO $ do fn <- run nextFileName
                         run (putStrLn fn)
                         f  <- run (writeFile (fn ++ ".hs")  (show m))
                         assert True

-- Function: prop_compilationPreservation
-- Objective: Tests if the generated programs are compiled by 'ghc' 
--            with different optimization levels.
-- Params: None.
-- Returns: True if all programs compile, False otherwise.
----------------------------------------------------------------------
prop_compilationPreservation = 
  forAll genModule $
    \m -> monadicIO $ do fn <- run nextFileName
                         run (putStrLn fn)
                         f  <- run (writeFile (fn ++ ".hs") (show m))
                         ex0 <- run (compileProgram fn "-O0")
                         ex' <- run (compileProgram fn "-O")
                         ex1 <- run (compileProgram fn "-O1")
                         ex2 <- run (compileProgram fn "-O2")
                         assert (ex0 == ExitSuccess && ex' == ExitSuccess && ex1 == ExitSuccess && ex2 == ExitSuccess)

-- Function: prop_behaviorPreservation
-- Objective: Tests if the generated programs behave the same way
--            when compiled with different optimization levels.
-- Params: None.
-- Returns: True if all programs behave the same way, False otherwise.
----------------------------------------------------------------------
prop_behaviorPreservation = 
  forAll genModule $
    \m -> monadicIO $ do fn <- run nextFileName
                         run (putStrLn fn)
                         f  <- run (writeFile (fn ++ ".hs") (show m))
                         ex0 <- run (compileProgram fn "-O0")
                         ex' <- run (compileProgram fn "-O")
                         ex1 <- run (compileProgram fn "-O1")
                         ex2 <- run (compileProgram fn "-O2")
                         case (ex0, ex', ex1, ex2) of
                           (ExitSuccess, ExitSuccess, ExitSuccess, ExitSuccess) -> do
                             (ex0', out0, err0) <- run (execProgram fn "-O0")
                             (ex', out, err) <- run (execProgram fn "-O")
                             (ex1', out1, err1) <- run (execProgram fn "-O1")
                             (ex2', out2, err2) <- run (execProgram fn "-O2")
                             case (ex0', ex', ex1', ex2') of
                               (ExitSuccess, ExitSuccess, ExitSuccess, ExitSuccess) -> do
                                  run (putStrLn ("Output: [" ++ out0 ++ "," ++ out ++ "," ++ out1 ++ "," ++ out2 ++ "]"))
                                  assert (out0 == out && out == out1 && out1 == out2)
                               _ -> do 
                                  run (putStrLn "Execution error.")
                                  assert False
                           _ -> do 
                                  run (putStrLn "Compilation error.")
                                  assert False

-- Function: main
-- Objective: Run all properties using QuickCheck.
--------------------------------------------------
main = do 
  let nt = 1000
  writeFile "gc.tmp" "0"
  quickCheckWithResult (stdArgs { maxSuccess = nt }) prop_behaviorPreservation