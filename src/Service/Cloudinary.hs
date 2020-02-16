module Service.Cloudinary
  ( CloudinaryResponse(..)
  , upload
  )
where

import           Env.Config

import           GHC.Generics

import           Data.Aeson                     ( FromJSON(..) )
import           Data.String.Conversions        ( cs )
import qualified Data.Text                     as T
import           Data.Tuple.Curry               ( uncurryN )

import           Control.Applicative

import           Network.HTTP.Req
import           Network.HTTP.Client.MultipartFormData
                                                ( partFileSource
                                                , partBS
                                                )

data CloudinaryResponse  = CloudinaryResponse {
  public_id     :: String,
  format        :: String,
  resource_type :: String
} deriving (Generic, FromJSON, Eq, Show)

upload :: Config -> FilePath -> Req (JsonResponse CloudinaryResponse)
upload config fileName = do
  let url     = uploadUrl config
      headers = buildHeaders config
  body <- buildReqBody config fileName
  req POST url body jsonResponse headers

buildReqBody Config { cdnUploadPreset } fileName =
  reqBodyMultipart
    $ [ partFileSource "file" fileName
      , partBS "upload_preset" (cs cdnUploadPreset)
      , partBS "resource_type" "auto"
      ]

buildHeaders Config { cdnCredentials } =
  header "Authorization" (cs cdnCredentials)
    <> header "Content-Type" "application/json"

uploadUrl Config { cdnCloudName } =
  https "api.cloudinary.com" /: "api" /: "v1" /: (cs cdnCloudName) /: "upload"
