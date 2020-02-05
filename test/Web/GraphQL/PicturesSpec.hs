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

setupFixtures :: Connection -> IO Connection
setupFixtures conn = do
  uuids <- insertPictures conn pictures
  _     <- insertPictureTags conn $ pictureTags uuids
  return conn

setup :: SpecWith (Connection, Application) -> Spec
setup = aroundApplication
  $ bracket (openConnection >>= setupFixtures) (cleanupDB >=> closeConnection)

-- Queries
----------------------------------------------------------------------

pictureQuery = [qq|
  query picture($fileName: String!) {
    picture(fileName: $fileName) {
      fileName
      mimeType
      url
    }
  }|]

randomPictureQuery = [qq|
  query randomPicture($tags: [String!]) {
    randomPicture(tags: $tags) {
      fileName
      mimeType
      url
    }
  }|]

picturesQuery = [qq|
  query pictures($tags: [String!], $pagination: PaginationInput) {
    pictures(tags: $tags, pagination: $pagination) {
      fileName
      tags
    }
  }|]

-- Tests
----------------------------------------------------------------------

spec :: Spec
spec = setup $ do
  describe "picture" $ do
    it "returns a picture with a valid file name" $ do
      postGQL pictureQuery [json|{fileName: "test1.jpg"}|]
        `shouldRespondWith` [json|{
              data: {
                picture: {
                  fileName: "test1.jpg",
                  mimeType: "image/jpg",
                  url: "test1.jpg"
                }
              }
            }|]

    it "returns null with an invalid file name" $ do
      postGQL pictureQuery [json|{fileName: "test0.jpg"}|]
        `shouldRespondWith` [json|{data: {picture: null}}|]

  describe "randomPicture" $ do
    it "returns some picture with a valid tag" $ do
      postGQL randomPictureQuery [json|{tags: ["a"]}|]
        `shouldRespondWith` 200
                              { matchBody = bodyContains "\"randomPicture\":{"
                              }

    it "returns null with an invalid tag" $ do
      postGQL randomPictureQuery [json|{tags: ["x"]}|]
        `shouldRespondWith` 200
                              { matchBody = bodyContains
                                              "\"randomPicture\":null"
                              }

  describe "pictures" $ do
    it "returns a list of pictures with a given tag" $ do
      postGQL picturesQuery [json|{tags: ["a"]}|] `shouldRespondWith` [json|{
              data: {
                pictures: [
                  {fileName: "test2.jpg", tags: ["a"]},
                  {fileName: "test1.jpg", tags: ["a", "b"]}
                ]
              }
            }|]

    it "returns all pictures when no tags are given" $ do
      postGQL picturesQuery [json|{}|] `shouldRespondWith` [json|{
              data: {
                pictures: [
                  {fileName: "test3.jpg", tags: ["b"]},
                  {fileName: "test2.jpg", tags: ["a"]},
                  {fileName: "test1.jpg", tags: ["a", "b"]}
                ]
              }
            }|]

main :: IO ()
main = hspec $ spec
