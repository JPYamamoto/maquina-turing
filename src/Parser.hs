{-# LANGUAGE DeriveGeneric #-}

module Parser ( Error (..)
              , Transition
              , parseMachine
              , parseInputWord
              ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap as M
import qualified Data.Hashable as H
import Data.Aeson ( decode, (.:), FromJSON(parseJSON), Value(Object), ToJSON )
import GHC.Generics
import Control.Monad ((>=>))

import Machine
import qualified Data.Functor as Parsers

-- Type declarations.

type Transition = [String]

-- | Data type that serves as a template for JSON parsing.
data MachineJSON =
  MachineJSON { jStates      :: [String]
              , jInput       :: [String]
              , jTape        :: [String]
              , jInitial     :: String
              , jBlank       :: String
              , jAccepting   :: [String]
              , jTransitions :: [Transition]
              } deriving (Show, Generic)

instance ToJSON MachineJSON
instance FromJSON MachineJSON where
 parseJSON (Object m) =
    MachineJSON <$> m .: "Estados"
                <*> m .: "Entrada"
                <*> m .: "Cinta"
                <*> m .: "Inicial"
                <*> m .: "Blanco"
                <*> m .: "Finales"
                <*> m .: "Transiciones"

data Error = InvalidJSON String
           | InvalidSpecification String
           | InvalidInput String

instance Show Error where
  show (InvalidJSON s)          = "Invalid JSON: " ++ s
  show (InvalidSpecification s) = "Invalid Machine Specification: " ++ s
  show (InvalidInput s)         = "Invalid Input Word: " ++ s

-- Parsers.

-- | Parse a string to a valid Machine.
-- | Return an error indicating the problem when parsing is not sucessful.
parseMachine :: B.ByteString -> Either Error Machine
parseMachine s = do
  machine <- case decode s of
    Nothing -> Left $ InvalidJSON "JSON couldn't be decoded"
    (Just m) -> return m

  validJson <- validJson machine
  return $ create machine

-- | Parse an input word to a list of symbols.
parseInputWord :: Machine -> String -> Either Error [Symbol]
parseInputWord m s = let symbols = map (Symbol . (:[])) s
                     in if validWord m symbols
                        then return symbols
                        else Left $ InvalidInput "Input word contains symbols not in input alphabet."

-- Validators.
validWord :: Machine -> [Symbol] -> Bool
validWord m = all (`elem` inputAlph m)

validJson :: MachineJSON -> Either Error MachineJSON
validJson = tapeContainsBlank
            >=> validInitial
            >=> validAccepting
            >=> validTransitions

tapeContainsBlank :: MachineJSON -> Either Error MachineJSON
tapeContainsBlank m
  | jBlank m `elem` jTape m = return m
  | otherwise = Left $ InvalidSpecification "The tape alphabet doesn't contain the blank character."

validInitial :: MachineJSON -> Either Error MachineJSON
validInitial m
  | jInitial m `elem` jStates m = return m
  | otherwise = Left $ InvalidSpecification "The initial state is not in the set of states."

validAccepting :: MachineJSON -> Either Error MachineJSON
validAccepting m = let xs = jStates m in
  if all (`elem` xs) (jAccepting m)
  then return m
  else Left $ InvalidSpecification "Accepting state not in set of states."

validTransitions :: MachineJSON -> Either Error MachineJSON
validTransitions m = if all f (jTransitions m)
                     then return m
                     else Left $ InvalidSpecification "Invalid transition."
  where f [s1, r, s2, w, a] = let tapeAlphabet  = jTape m ++ jInput m
                                  statesMachine = jStates m
                              in s1 `elem` statesMachine &&
                                 s2 `elem` statesMachine &&
                                 r  `elem` tapeAlphabet &&
                                 w  `elem` tapeAlphabet &&
                                 a  `elem` ["R", "L", "N"]
        f _ = False

-- Generators.

-- | Turn a valid JSON parsed machine, to a Machine
-- | usable in the program context.
create :: MachineJSON -> Machine
create m =
  Machine (parseStates      (jStates m))
          (parseInput       (jInput m))
          (parseTape        (jTape m))
          (parseInitial     (jInitial m))
          (parseBlank       (jBlank m))
          (parseAccepting   (jAccepting m))
          (parseTransitions (jTransitions m))

parseState :: String -> State
parseState = State

parseSymbol :: String -> Symbol
parseSymbol = Symbol

parseStates :: [String] -> [State]
parseStates = map parseState

parseInput :: [String] -> [Symbol]
parseInput = map parseSymbol

parseTape :: [String] -> [Symbol]
parseTape = map parseSymbol

parseInitial :: String -> State
parseInitial = parseState

parseBlank :: String -> Symbol
parseBlank = parseSymbol

parseAccepting :: [String] -> [State]
parseAccepting = map parseState

parseTransitions :: [Transition] -> M.Map Argument Result
parseTransitions = M.fromList . map f
  where f ~[s1, r, s2, w, a] = let arg = (parseState s1, parseSymbol r)
                                   res = (parseState s2, parseSymbol w, read a)
                               in (arg, res)
