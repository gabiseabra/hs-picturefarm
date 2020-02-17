module Service.CloudinarySpec where

import           Spec.HttpCase

import           Env
import           Service.Cloudinary

import           Data.Default.Class
import           Data.Aeson.QQ                  ( aesonQQ )

import           Control.Applicative

import           Network.HTTP.Req               ( runReq
                                                , responseBody
                                                )
import qualified Web.Scotty                    as S

-- Set up
--------------------------------------------------------------------------------

cfg :: Config
cfg = def { cdnCloudName    = "test"
          , cdnUploadPreset = "test"
          , cdnApiSecret    = "test"
          , cdnApiKey       = "test"
          }

mockCloudinaryServer = do
  S.post "/v1_1/test/image/upload" $ do
    S.json [aesonQQ|{public_id: "test", format: "gif", resource_type: "image"}|]
  S.post "/v1_1/test/video/upload" $ do
    S.json
      [aesonQQ|{public_id: "test", format: "webm", resource_type: "video"}|]

setup :: SpecWith () -> Spec
setup = withMockServer mockCloudinaryServer

-- Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = setup $ do
  describe "upload" $ do
    it "uploads image file" $ do
      r <- mockReq (upload cfg "test/fixtures/tiny.gif")
      (responseBody r) `shouldBe` CloudinaryResponse { public_id     = "test"
                                                     , format        = "gif"
                                                     , resource_type = "image"
                                                     }

    it "uploads image file" $ do
      r <- mockReq (upload cfg "test/fixtures/tiny.webm")
      (responseBody r) `shouldBe` CloudinaryResponse { public_id     = "test"
                                                     , format        = "webm"
                                                     , resource_type = "video"
                                                     }

main :: IO ()
main = hspec $ spec
