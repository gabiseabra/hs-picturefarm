module Spec.ConnCase (
  module Spec.TestCase,
  module Database.PostgreSQL.Simple,
  module Database.PostgreSQL.Simple.SqlQQ,
  openConnection,
  closeConnection,
  md5
) where

import Spec.TestCase

import Control.Monad.IO.Class  (liftIO)
import Database.PostgreSQL.Simple

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Crypto.Hash.MD5 as MD5

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Config

openConnection :: IO Connection
openConnection = loadConfig >>= createConnection

closeConnection :: Connection -> IO ()
closeConnection conn = close conn

md5 :: String -> T.Text
md5 a = T.pack $ B.unpack $ MD5.hash $ B.pack a