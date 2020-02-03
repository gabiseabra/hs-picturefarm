module Web.GraphQL.PicturesSpec where

import           Spec.WebCase
import           Spec.ModelCase

import           Data.UUID                      ( UUID )

import           Control.Arrow
import           Control.Composition

-- Set up fixtures
----------------------------------------------------------------------

pictures :: [PictureInput]
pictures =
  [ ("test1.jpg", "test1.jpg", md5 "test1", "image/jpg")
  , ("test2.jpg", "test2.jpg", md5 "test2", "image/jpg")
  , ("test3.jpg", "test3.jpg", md5 "test3", "image/jpg")
  ]

pictureTags :: [UUID] -> [PictureTagInput]
pictureTags uuids = zip uuids [["a", "b"], ["a"], ["b"]]

setupFixtures :: IO ()
setupFixtures = withConnection $ \conn -> do
  uuids <- insertPictures conn pictures
  _     <- insertPictureTags conn $ pictureTags uuids
  return ()

setup :: SpecWith Application -> Spec
setup = before_ setupFixtures . withApplication

-- Queries
----------------------------------------------------------------------

picturesQuery = [qq|
  query pictures($uuid: UUID!) {
    pictures(uuid: $uuid) {
      uuid
      fileNames
    }
  }|]

-- Tests
----------------------------------------------------------------------

spec :: Spec
spec = setup $ do
  describe "picture" $ do
    it "returns a picture" $ do
      postGQL picturesQuery [json|{uuid: ""}|] `shouldRespondWith` [json|{
              data: {
                
              }
            }|]

main :: IO ()
main = hspec $ spec
