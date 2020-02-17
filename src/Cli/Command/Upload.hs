{-# LANGUAGE ScopedTypeVariables #-}

module Cli.Command.Upload
  ( main
  , prog
  , parser
  )
where

import           Env
import           Service.Cloudinary             ( CloudinaryResponse(..) )
import qualified Service.Cloudinary            as CDN
import           Model.Picture                  ( Picture(..) )
import qualified Model.Picture                 as Pic

import           Foreign.C.Error                ( Errno )

import qualified Crypto.Hash.MD5               as MD5
import           Data.Either.Combinators        ( rightToMaybe )
import           Data.Maybe                     ( maybe
                                                , fromMaybe
                                                )
import           Data.UUID                      ( UUID )
import qualified Data.UUID                     as UUID
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Base16        as BS16
import qualified Data.Text                     as T
import           Data.String.Conversions        ( cs )

import           Control.Monad                  ( void
                                                , guard
                                                , liftM
                                                )
import           Control.Exception              ( tryJust
                                                , catch
                                                )
import           Options.Applicative

import           System.FilePath.Posix          ( takeFileName )
import           System.Xattr                   ( XattrMode(..)
                                                , getxattr
                                                , setxattr
                                                )

import           Network.HTTP.Req               ( runReq
                                                , defaultHttpConfig
                                                , responseBody
                                                )

main :: [FilePath] -> IO ()
main files = do
  env <- initialize >>= getEnv
  void . traverse (processFile env) $ files

-- | Process each file
--------------------------------------------------------------------------------
processFile :: Env -> FilePath -> IO ()
processFile env file = do
  pic <- findByFile env file
  case pic of
    Nothing -> do
      Picture { uuid, fileName } <- insertPicture env file
      putStrLn $ "[" ++ (show uuid) ++ "] Inserted " ++ (cs fileName)
    Just pic@Picture { uuid, fileName } -> do
      uploadPicture env file pic
      putStr $ "[" ++ (show uuid) ++ "] Updated " ++ (cs fileName)

-- | Upload and insert a new picture to the database
--------------------------------------------------------------------------------
insertPicture :: Env -> FilePath -> IO Picture
insertPicture Env { conn, config } file = do
  fileHash <- md5Digest file
  res      <- runReq defaultHttpConfig (responseBody <$> CDN.upload config file)
  let pic = Picture { id       = 0
                    , uuid     = UUID.nil
                    , fileHash
                    , fileName = (cs $ takeFileName file)
                    , url      = public_id res <> "." <> format res
                    , mimeType = resource_type res <> "/" <> format res
                    , tags     = []
                    }
  (rid, uuid) <- Pic.insertPicture conn pic
  _           <- setxattrUUID file uuid
  return pic { Pic.id = rid, uuid }

-- | Updates picture attributes
--------------------------------------------------------------------------------
uploadPicture :: Env -> FilePath -> Picture -> IO ()
uploadPicture Env { conn } file pic = do
  fileHash <- md5Digest file
  Pic.updatePicture conn pic { fileHash, fileName = (cs $ takeFileName file) }

-- | Try to match a FilePath to one picture on the database
--------------------------------------------------------------------------------
findByFile :: Env -> FilePath -> IO (Maybe Picture)
findByFile env file =
  (getByUUID env file) >>= maybe (getByFileHash env file) (return . Just)

-- | Lookup by file hash
getByFileHash :: Env -> FilePath -> IO (Maybe Picture)
getByFileHash Env { conn } file =
  md5Digest file >>= Pic.getPictureBy conn Pic.FileHash

-- | Lookup by xattr UUID
getByUUID :: Env -> FilePath -> IO (Maybe Picture)
getByUUID Env { conn } file =
  getxattrUUID file >>= maybe (return Nothing) (Pic.getPictureBy conn Pic.UUID)

-- File attr helpers
--------------------------------------------------------------------------------

getxattrUUID :: FilePath -> IO (Maybe UUID)
getxattrUUID file =
  catch (getxattr file "picturefarm-uuid") (\(_ :: IOError) -> return "")
    >>= return
    .   UUID.fromASCIIBytes

setxattrUUID :: FilePath -> UUID -> IO ()
setxattrUUID file uuid = setxattr file "picturefarm-uuid" (cs uuid) RegularMode

md5Digest :: FilePath -> IO T.Text
md5Digest = liftM (cs . BS16.encode . MD5.hash) . BS.readFile

-- Option parser
--------------------------------------------------------------------------------

prog :: ParserInfo (IO ())
prog = info (main <$> parser) (progDesc "Upload files on cloudinary")

parser :: Parser [FilePath]
parser = helper
  <*> many (strArgument (metavar "TARGET" <> help "Path of a file to upload"))
