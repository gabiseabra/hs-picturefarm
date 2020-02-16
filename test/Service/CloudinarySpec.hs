module Service.CloudinarySpec where

import           Spec.HttpCase
import           Test.Hspec.Wai.JSON            ( json )
import           Data.Aeson.QQ                  ( aesonQQ )
import           Env
import           Service.Cloudinary

import           Control.Applicative

import           Network.HTTP.Req               ( runReq
                                                , responseBody
                                                )
import qualified Web.Scotty                    as Scotty

-- Tests
----------------------------------------------------------------------

mockCloudinaryServer = do
  Scotty.post "/api/v1/:name/upload" $ do
    Scotty.json
      [aesonQQ|{public_id: "test", format: "gif", resource_type: "image"}|]

setup :: SpecWith Config -> Spec
setup = withMockServer mockCloudinaryServer . before (loadConfig (Just Test))

spec :: Spec
spec = setup $ do
  describe "upload" $ do
    it "uploads picture to cloudinary" $ \cfg -> do
      r <- mockReq (upload cfg "test/fixtures/tiny.gif")
      (responseBody r) `shouldBe` CloudinaryResponse { public_id     = "test"
                                                     , format        = "gif"
                                                     , resource_type = "image"
                                                     }

main :: IO ()
main = hspec $ spec
