module Spec.ConnCase
  ( module Spec.TestCase
  , Connection
  , openConnection
  , closeConnection
  , md5
  )
where

import           Env                            ( createConnection )
import           Spec.Config
import           Spec.TestCase

import           Control.Monad.IO.Class         ( liftIO )

import qualified Data.Text                     as T
import qualified Data.ByteString.Char8         as B
import qualified Crypto.Hash.MD5               as MD5
import           Data.String.Conversions

import           Database.PostgreSQL.Simple     ( Connection
                                                , close
                                                )
import           Database.PostgreSQL.Simple.SqlQQ

-- Test helpers
----------------------------------------------------------------------

openConnection :: IO Connection
openConnection = testConfig >>= createConnection

closeConnection = close

md5 :: String -> T.Text
md5 a = cs $ B.unpack $ MD5.hash $ B.pack a
