module Model.PictureSpec where

import           Spec.ModelCase

import           Model.Pagination
import           Model.Picture

import           Data.UUID
import qualified Data.UUID.V4                  as UUIDv4
import           Data.List
import           Data.Text                      ( Text )
import           Data.String.QM

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
pictureTags uuids = zip uuids [["a", "b", "d"], ["a'", "d"], ["c", "d"], ["d"]]

tagAliases :: [TagAliasInput]
tagAliases = [("a", "a'")]

setupFixtures :: Connection -> IO (Connection, [UUID])
setupFixtures conn = do
  uuids <- insertPictures conn pictures
  _     <- insertPictureTags conn $ pictureTags uuids
  _     <- insertTagAliases conn tagAliases
  return (conn, uuids)

hook :: ((Connection, [UUID]) -> IO ()) -> IO ()
hook = bracket (openConnection >>= setupFixtures)
               ((cleanupDB >=> closeConnection) <<< fst)

-- Tests
----------------------------------------------------------------------

spec :: Spec
spec = around hook $ do
  describe "getByUuid" $ do
    it "returns one picture with valid uuid" $ \(conn, (actual_uuid : _)) -> do
      getByUuid actual_uuid conn
        >>= (`shouldBeRightAnd` ((== actual_uuid) . uuid))

    it "returns NotFound with invalid uuid" $ \(conn, _) -> do
      UUIDv4.nextRandom
        >>= flip getByUuid conn
        >>= (`shouldBeLeftAnd` (== NotFound))

  describe "findByTags" $ do
    it "queries pictures with a given tag" $ \(conn, _) -> do
      findByTags (["a"], Nothing, UpdatedAt) conn
        >>= (`shouldBeRightAnd` ( (== ["test1.jpg", "test2.jpg"])
                                . sort
                                . map fileName
                                )
            )

    it "queries pictures with any of the given tag" $ \(conn, _) -> do
      findByTags (["b", "c"], Nothing, UpdatedAt) conn
        >>= (`shouldBeRightAnd` ( (== ["test1.jpg", "test3.jpg"])
                                . sort
                                . map fileName
                                )
            )

    it "paginates results" $ \(conn, _) -> do
      findByTags
          ( ["d"]
          , Just (PaginationInput { page = Nothing, pageSize = Just 2 })
          , UpdatedAt
          )
          conn
        >>= (`shouldBeRightAnd` ((== 2) . length))
      findByTags
          ( ["d"]
          , Just (PaginationInput { page = Just 2, pageSize = Just 2 })
          , UpdatedAt
          )
          conn
        >>= (`shouldBeRightAnd` ((== 1) . length))
      findByTags
          ( ["d"]
          , Just (PaginationInput { page = Just 3, pageSize = Just 2 })
          , UpdatedAt
          )
          conn
        >>= (`shouldBeRightAnd` ((== 0) . length))

main :: IO ()
main = hspec $ spec
