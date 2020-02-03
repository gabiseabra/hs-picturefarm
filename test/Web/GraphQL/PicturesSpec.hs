module Web.GraphQL.PicturesSpec where

import           Spec.WebCase

-- Tests
----------------------------------------------------------------------

picturesQuery = [qq|
  query pictures($uuid: UUID!) {
    pictures(uuid: $uuid) {
      uuid
      fileNames
    }
  }|]

spec :: Spec
spec = withApplication $ do
  describe "picture" $ do
    it "returns a picture" $ do
      postGQL picturesQuery [json|{uuid: "098098-12345"}|]
        `shouldRespondWith` [json|{
          data: {}
        }|]

main :: IO ()
main = hspec $ spec
