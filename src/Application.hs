module Application (app) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Pool
import Database.PostgreSQL.Simple

import Config
import Model.Picture

app :: IO ()
app = do
  init <- runExceptT initialize
  case init of
    Left err -> putStrLn err
    Right config -> runServer config

runServer :: (Config, Pool Connection) -> IO ()
runServer (config, pool) = do
  x <- withResource pool $ findByTags ["foo", "test"]
  print x