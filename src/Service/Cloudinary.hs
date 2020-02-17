module Service.Cloudinary
  ( CloudinaryResponse(..)
  , upload
  )
where

import           Env.Config

import           GHC.Generics

import qualified Crypto.Hash.SHA1              as SHA1
import qualified Data.Text                     as T
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Base16        as BS16
import           Data.Aeson                     ( FromJSON(..) )
import           Data.String.Conversions        ( cs )
import           Data.Tuple.Curry               ( uncurryN )
import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import           Data.List.Split                ( splitOn )

import           Control.Applicative
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )

import           Network.Mime                   ( defaultMimeLookup )
import           Network.HTTP.Req
import           Network.HTTP.Client.MultipartFormData
                                                ( partFileSource
                                                , partBS
                                                )

data CloudinaryResponse  = CloudinaryResponse {
  public_id     :: T.Text,
  format        :: T.Text,
  resource_type :: T.Text
} deriving (Generic, FromJSON, Eq, Show)

upload :: Config -> FilePath -> Req (JsonResponse CloudinaryResponse)
upload config fileName = do
  let url     = uploadUrl config fileName
      headers = buildHeaders config
  body <- buildReqBody config fileName
  req POST url body jsonResponse headers

buildReqBody cfg@Config { cdnUploadPreset, cdnApiKey } fileName = do
  timestamp <- now
  let sig = signUpload cfg timestamp
  reqBodyMultipart
    [ partFileSource "file" fileName
    , partBS "api_key"       (cs cdnApiKey)
    , partBS "upload_preset" (cs cdnUploadPreset)
    , partBS "resource_type" (resourceType fileName)
    , partBS "timestamp"     (cs $ show timestamp)
    , partBS "signature"     (cs sig)
    ]

buildHeaders Config { cdnApiKey, cdnApiSecret } =
  basicAuth (cs cdnApiKey) (cs cdnApiSecret)

uploadUrl Config { cdnCloudName } fileName =
  https "api.cloudinary.com"
    /: "v1_1"
    /: (cs cdnCloudName)
    /: (cs $ resourceType fileName)
    /: "upload"

resourceType :: String -> BS.ByteString
resourceType = head . BS.split '/' . defaultMimeLookup . cs

signUpload :: Config -> Int -> String
signUpload Config { cdnUploadPreset, cdnApiSecret } timestamp =
  sha1
    $  "timestamp="
    <> (show timestamp)
    <> "&upload_preset="
    <> cdnUploadPreset
    <> cdnApiSecret

now :: (MonadIO m) => m Int
now = round . (1000 *) <$> liftIO getPOSIXTime

sha1 = cs . BS16.encode . SHA1.hash . cs
