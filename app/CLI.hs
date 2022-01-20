module CLI (Program (..), parseCLI) where

import Options.Applicative
import Data.Semigroup ((<>))

-- The data type that represents the arguments of the CLI arguments.
data Program = Program { file  :: Maybe String
                       , input :: Maybe String
                       }

-- | Parser for the CLI flags.
parseCLI :: Parser Program
parseCLI = Program
  <$> optional (strOption (short 'f' <> long "file"  <> metavar "FILE"  <> help fileHelp))
  <*> optional (strOption (short 'i' <> long "input" <> metavar "INPUT" <> help inputHelp))
  where fileHelp =  "JSON file with the Turing Machine specification."
        inputHelp = "The input to feed the Turing Machine."
