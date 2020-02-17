module Spec.ConnCase
  ( module Spec.TestCase
  , Connection
  , openConnection
  , closeConnection
  , md5
  )
where

import           Env
import           Spec.TestCase

import           Control.Monad.IO.Class         ( liftIO )

import qualified Data.Text                     as T
import qualified Data.ByteString.Char8         as B
import qualified Crypto.Hash.MD5               as MD5
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Base16        as BS16
import           Data.String.Conversions
import           Data.Default.Class

import           Database.PostgreSQL.Simple     ( Connection
                                                , close
                                                )
import           Database.PostgreSQL.Simple.SqlQQ

-- Test helpers
----------------------------------------------------------------------

cfg :: Config
cfg = def
  { databaseUrl =
    "postgres://postgres:postgres@localhost:5432/picturefarm_test?"
      <> "sslmode=disable"
  }

openConnection :: IO Connection
openConnection = createConnection cfg

closeConnection = close

md5 :: String -> T.Text
md5 = cs . BS16.encode . MD5.hash . cs
