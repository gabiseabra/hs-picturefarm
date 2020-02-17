

module Cli
  ( main
  )
where

import qualified Cli.Command                   as Cmd

import           Control.Monad                  ( join )

import           Options.Applicative

main :: IO ()
main = join $ execParser Cmd.prog
