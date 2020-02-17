module System.IO.MetaData
  ( getxattrUUID
  , setxattrUUID
  , getxattrTags
  , md5Digest
  )
where

import           Database.QueryBuilder -- String conversions

import qualified Crypto.Hash.MD5               as MD5
import           Data.UUID                      ( UUID )
import qualified Data.UUID                     as UUID
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Base16        as BS16
import qualified Data.Text                     as T
import           Data.String.Conversions        ( cs )
import           Data.List.Split                ( splitOn )

import           Control.Exception              ( catch )
import           Control.Monad                  ( liftM )

import           System.Command
import           System.Xattr                   ( XattrMode(..)
                                                , getxattr
                                                , setxattr
                                                )

getxattrTags :: FilePath -> IO [T.Text]
getxattrTags file = do
  Stdout out <- command [] "tag" ["-0lN", file]
  return . map cs . splitOn "," . init $ out

getxattrUUID :: FilePath -> IO (Maybe UUID)
getxattrUUID file =
  catch (getxattr file "picturefarm-uuid") (\(_ :: IOError) -> return "")
    >>= return
    .   UUID.fromASCIIBytes

setxattrUUID :: FilePath -> UUID -> IO ()
setxattrUUID file uuid = setxattr file "picturefarm-uuid" (cs uuid) RegularMode

md5Digest :: FilePath -> IO T.Text
md5Digest = liftM (cs . BS16.encode . MD5.hash) . BS.readFile
