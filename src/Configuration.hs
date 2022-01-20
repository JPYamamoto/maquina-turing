module Configuration ( Configuration (..)
                     , initialConfig
                     , step
                     , acceptConfig
                     ) where

import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Data.HashMap as M

import Machine
import Tape

-- Type Declarations

-- | Data type for a Configuration.
-- | A configuration represents a specific state in time of the
-- | system.
data Configuration = Configuration { machine :: Machine
                                   , state   :: State
                                   , tape    :: Tape Symbol
                                   }

-- | Print Configurations on screen.
instance Show Configuration where
  show (Configuration _ s (Tape ls m rs)) = "..."
                                            ++ f (reverse (take 20 ls))
                                            ++ "| [" ++ show s
                                            ++ " -> " ++ show m ++ "] |"
                                            ++ f (take 20 rs) ++ "..."
    where f = intercalate "|" . map show


-- | Declare an initial configuration, by embedding the input word
-- | into the tape of the machine.
initialConfig :: Machine -> [Symbol] -> Configuration
initialConfig m s = let blankTape = repeat (blankSymbol m)
                        (t:ts)    = s ++ blankTape
                        mTape     = Tape blankTape t ts
                    in Configuration m (initialState m) mTape

-- Machine Actions

-- | Decides if the current configuration is in an accepting
-- | state.
acceptConfig :: Configuration -> Bool
acceptConfig c = state c `elem` acceptingStates (machine c)

-- | Perform a single step in the machine.
-- | This action generates a new configuration.
step :: Configuration -> Maybe Configuration
step (Configuration m s t) = do
  (newState, newSymbol, movement) <- delta m (s, look t)
  let newTape = move movement (write newSymbol t)
    in return $ Configuration m newState newTape

-- | Read the symbol in the tape.
look :: Tape Symbol -> Symbol
look = get

-- | Write a symbol to the tape.
write :: Symbol -> Tape Symbol -> Tape Symbol
write = put

-- | Move in the tape according to the instruction.
-- | This is a safe operation because the tape is infinite in
-- | both directions.
move :: Movement -> Tape a -> Tape a
move N = id
move L = fromJust . moveLeft
move R = fromJust . moveRight

-- | Simulate the delta function.
delta :: Machine -> Argument -> Maybe Result
delta m arg = M.lookup arg (transitions m)
