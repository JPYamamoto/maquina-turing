{-# LANGUAGE DeriveGeneric #-}

module Machine ( Machine (..)
               , State (..)
               , Symbol (..)
               , Argument
               , Result
               , Movement (..)
               ) where

import GHC.Generics
import Data.List (intercalate)
import qualified Data.HashMap as M
import qualified Data.Hashable as H

-- Type declarations.
newtype State  = State  String deriving (Generic, Eq, Ord)
newtype Symbol = Symbol String deriving (Generic, Eq, Ord)
data Movement  = L | R | N deriving (Show, Read, Eq)
type Argument  = (State, Symbol)
type Result    = (State, Symbol, Movement)

instance H.Hashable State
instance H.Hashable Symbol

data Machine =
  Machine { states          :: [State]
          , inputAlph       :: [Symbol]
          , tapeAlph        :: [Symbol]
          , initialState    :: State
          , blankSymbol     :: Symbol
          , acceptingStates :: [State]
          , transitions     :: M.Map Argument Result
          }

-- Show derivations.
instance Show State where
  show (State s) = s

instance Show Symbol where
  show (Symbol s) = s

instance Show Machine where
  show m = "M = (Q={" ++ format (states m) ++ "}, Σ={"
           ++ format (inputAlph m)  ++ "}, Γ={"
           ++ format (tapeAlph m)   ++ "}, "
           ++ show (initialState m) ++ ", "
           ++ show (blankSymbol m)  ++ ", F={"
           ++ format (acceptingStates m) ++ "}, δ={"
           ++ format (M.toList (transitions m)) ++ "})"
        where
          format :: Show a => [a] -> String
          format  = intercalate ", " . map show
