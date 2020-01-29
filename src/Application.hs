module Application (app) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import Data.Pool (withResource)

import Config
import Model.Picture

app :: IO ()
app = initialize >>= runServer

runServer :: (Config, Pool Connection) -> IO ()
runServer (config, pool) = do
  x <- withResource pool $ findByTags ["foo", "test"]
  print x