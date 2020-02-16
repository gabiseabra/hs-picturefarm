module Service.CloudinarySpec where

import           Spec.HttpCase

import           Env
import           Service.Cloudinary

import           Data.Aeson.QQ                  ( aesonQQ )

import           Control.Applicative

import           Network.HTTP.Req               ( runReq
                                                , responseBody
                                                )
import qualified Web.Scotty                    as S

-- Set up
--------------------------------------------------------------------------------

mockCloudinaryServer = do
  S.post "/api/v1/:name/upload" $ do
    S.json [aesonQQ|{public_id: "test", format: "gif", resource_type: "image"}|]

setup :: SpecWith Config -> Spec
setup = withMockServer mockCloudinaryServer . before (loadConfig (Just Test))

-- Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = setup $ do
  describe "upload" $ do
    it "uploads file on cloudinary" $ \cfg -> do
      r <- mockReq (upload cfg "test/fixtures/tiny.gif")
      (responseBody r) `shouldBe` CloudinaryResponse { public_id     = "test"
                                                     , format        = "gif"
                                                     , resource_type = "image"
                                                     }

main :: IO ()
main = hspec $ spec
