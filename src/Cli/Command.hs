

module Cli.Command
  ( prog
  )
where

import qualified Cli.Command.Upload            as Upload
import           Options.Applicative

prog :: ParserInfo (IO ())
prog = info parser idm

parser :: Parser (IO ())
parser = helper <*> subparser (command "upload" Upload.prog)
