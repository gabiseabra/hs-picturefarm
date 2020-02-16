module Cli.Command.Upload
  ( main
  , prog
  , parser
  )
where

import           Env
import           Service.Cloudinary

import           Control.Monad                  ( void )

import           Options.Applicative

import           Network.HTTP.Req               ( runReq
                                                , defaultHttpConfig
                                                , responseBody
                                                )

main :: [FilePath] -> IO ()
main files = do
  config <- getEnvironment >>= loadConfig
  void . traverse (doUpload config) $ files

doUpload config file = do
  res <- runReq defaultHttpConfig (upload config file)
  print (responseBody res)

-- Option parser
--------------------------------------------------------------------------------

prog :: ParserInfo (IO ())
prog = info (main <$> parser) (progDesc "Upload files on cloudinary")

parser :: Parser [FilePath]
parser = helper
  <*> many (strArgument (metavar "TARGET" <> help "Path of a file to upload"))
