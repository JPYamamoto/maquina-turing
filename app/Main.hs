module Main (main) where

import qualified Data.ByteString.Lazy as B
import Options.Applicative
import Data.Maybe (fromJust)
import Control.Concurrent

import CLI
import qualified Parser as P
import qualified Machine as M
import qualified Configuration as C
import qualified Parser as P
import System.IO (hFlush, stdout)

-- | Entry point of the program.
-- | The input can be passed to the program using the flags:
-- |   - `--file` : The JSON file with the machine specification.
-- |   - `--input` : The word passed as input to the Turing Machine.
-- |
-- | If either of the previous flags is not given, the program will
-- | ask for the missing data through the standard input.
main :: IO ()
main = main' =<< execParser opts
  where
    opts = info (parseCLI <**> helper)
      (fullDesc
       <> progDesc "Read a Turing Machine specified as a JSON and its input, and run the Universal Turing Machine."
       <> header   "Turing Machine - a universal Turing Machine")

main' :: Program -> IO ()
main' (Program Nothing i) = do     -- Missing JSON file.
  putStr "Enter JSON file path: "
  hFlush stdout
  filepath <- getLine
  main' $ Program (Just filepath) i

main' (Program f Nothing) = do     -- Missing input word.
  putStr "Enter input word: "
  hFlush stdout
  input <- getLine
  main' $ Program f (Just input)

main' (Program (Just file) (Just input)) = do -- Complete data.
  contents <- B.readFile file

  case parseInput contents input of
    (Left error)   -> putStrLn $ "Error when parsing JSON file:\n" ++ show error
    (Right config) -> do
      putStrLn $ "Received machine: " ++ show (C.machine config)
      putStrLn $ "Received input: " ++ input
      putStrLn "Starting execution:"
      runUntilHalt config

-- | Parse the machine and input word into a valid initial configuration.
parseInput :: B.ByteString -> String -> Either P.Error C.Configuration
parseInput m w = do
  machine <- P.parseMachine m
  inputWord <- P.parseInputWord machine w
  return $ C.initialConfig machine inputWord

-- | If the configuration has halted, decide whether the input is accepted.
-- | Otherwise, delay the execution by 1/4 of a second, and execute one step.
runUntilHalt :: C.Configuration -> IO ()
runUntilHalt c = do
  print c

  case C.step c of
    Nothing -> halted c
    Just c' -> threadDelay 250000 >> runUntilHalt c'

-- | Check if the current configuration is in a final state.
halted :: C.Configuration -> IO ()
halted c
  | C.acceptConfig c = putStrLn "Input accepted."
  | otherwise        = putStrLn "Input not accepted."
