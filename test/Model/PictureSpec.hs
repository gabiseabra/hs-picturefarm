module Model.PictureSpec where

import           Spec.ModelCase


import           Model.Pagination
import           Model.Picture

import           Data.Default.Class
import           Data.UUID
import qualified Data.UUID.V4                  as UUIDv4
import           Data.List
import           Data.Text                      ( Text )
import           Data.String.QM
import           Data.Either.Combinators

import           Control.Monad
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
pictureTags uuids = zip uuids [["a", "b", "d"], ["a'", "d"], ["c", "d"]]

tagAliases :: [TagAliasInput]
tagAliases = [("a", "a'")]

setupFixtures :: Connection -> IO (Connection, [UUID])
setupFixtures conn = do
  uuids <- insertPictures conn pictures
  _     <- insertPictureTags conn $ pictureTags uuids
  _     <- insertTagAliases conn tagAliases
  return (conn, uuids)

setup :: SpecWith (Connection, [UUID]) -> Spec
setup = around $ bracket (openConnection >>= setupFixtures)
                         ((cleanupDB >=> closeConnection) <<< fst)

-- Helpers
----------------------------------------------------------------------

mapIds :: Either a [Picture] -> [UUID]
mapIds = (map uuid) . (fromRight [])

-- Tests
----------------------------------------------------------------------

spec :: Spec
spec = setup $ do
  describe "getPictureBy" $ do
    it "returns one picture with valid uuid" $ \(conn, (actual_uuid : _)) -> do
      getPictureBy UUID actual_uuid conn
        >>= (`shouldBeRightAnd` ((== Just actual_uuid) . liftM uuid))

    it "returns NotFound with invalid uuid" $ \(conn, _) -> do
      UUIDv4.nextRandom
        >>= flip (getPictureBy UUID) conn
        >>= (`shouldBeRightAnd` (== Nothing))

  describe "findPictures" $ do
    it "queries pictures with a given tag" $ \(conn, _) -> do
      findPictures def { tags = Just ["a"] } conn
        >>= (`shouldBeRightAnd` ( (== ["test1.jpg", "test2.jpg"])
                                . sort
                                . map fileName
                                )
            )

    it "queries pictures with any of the given tag" $ \(conn, _) -> do
      findPictures def { tags = Just ["b", "c"] } conn
        >>= (`shouldBeRightAnd` ( (== ["test1.jpg", "test3.jpg"])
                                . sort
                                . map fileName
                                )
            )

    it "orders results" $ \(conn, _) -> do
      desc <- findPictures
        def { tags = Just ["a"], orderBy = OrderBy FileName DESC }
        conn
      asc <- findPictures
        def { tags = Just ["a"], orderBy = OrderBy FileName ASC }
        conn
      (mapIds desc) `shouldBe` (reverse $ mapIds asc)

    it "paginates results" $ \(conn, _) -> do
      findPictures
          def
            { tags       = Just ["d"]
            , pagination =
              Just (PaginationInput { page = Nothing, pageSize = Just 2 })
            }
          conn
        >>= (`shouldBeRightAnd` ((== 2) . length))
      findPictures
          def
            { tags       = Just ["d"]
            , pagination =
              Just (PaginationInput { page = Just 2, pageSize = Just 2 })
            }
          conn
        >>= (`shouldBeRightAnd` ((== 1) . length))
      findPictures
          def
            { tags       = Just ["d"]
            , pagination =
              Just (PaginationInput { page = Just 3, pageSize = Just 2 })
            }
          conn
        >>= (`shouldBeRightAnd` ((== 0) . length))

main :: IO ()
main = hspec $ spec
