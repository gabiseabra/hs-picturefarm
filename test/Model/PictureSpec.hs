module Model.PictureSpec where

import           Spec.ModelCase


import           Model.Pagination
import           Model.Picture
import qualified Model.Picture                 as Pic

import           Data.Default.Class
import           Data.UUID                      ( UUID )
import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUIDv4
import           Data.List
import           Data.Text                      ( Text )
import           Data.String.QM
import           Data.Either.Combinators

import           Control.Monad
import           Control.Arrow
import           Control.Composition

import           Database.PostgreSQL.Simple     ( Only(..)
                                                , query
                                                )
import           Database.PostgreSQL.Simple.SqlQQ

-- Set up fixtures
----------------------------------------------------------------------

newPic = Picture 0
                 UUID.nil
                 "test.jpg"
                 (md5 "test")
                 "test.jpg"
                 "image/jpg"
                 ["a", "b", "c"]

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
  describe "insertPicture" $ do
    it "inserts a picture on the database" $ \(conn, _) -> do
      (rid, uuid) <- insertPicture conn newPic
      pic         <- getPictureBy conn UUID uuid
      case pic of
        Just pic -> pic `shouldBe` newPic { Pic.id = rid, uuid = uuid }
        Nothing  -> expectationFailure "Picture was not inserted"

  describe "updatePicture" $ do
    it "updates a picture on the database" $ \(conn, (uuid : _)) -> do
      picM <- getPictureBy conn UUID uuid
      case picM of
        Just pic -> do
          let nextPic = pic { fileName = "eyy.lmao", tags = ["x"] }
          updatePicture conn nextPic
          getPictureBy conn UUID uuid >>= (`shouldBe` Just nextPic)
        Nothing -> expectationFailure "Inserted picture was not found"

  describe "getPictureBy" $ do
    it "returns one picture with valid uuid" $ \(conn, (actual_uuid : _)) -> do
      getPictureBy conn UUID actual_uuid >>= maybe
        (expectationFailure "Picture was not found")
        ((`shouldBe` actual_uuid) . uuid)

    it "returns Nothing with invalid uuid" $ \(conn, _) -> do
      UUIDv4.nextRandom >>= getPictureBy conn UUID >>= (`shouldBe` Nothing)

  describe "findPictures" $ do
    it "queries pictures with a given tag" $ \(conn, _) -> do
      findPictures conn def { tags = Just ["a"] }
        >>= (`shouldBe` ["test1.jpg", "test2.jpg"])
        .   sort
        .   map fileName


    it "queries pictures with any of the given tag" $ \(conn, _) -> do
      findPictures conn def { tags = Just ["b", "c"] }
        >>= (`shouldBe` ["test1.jpg", "test3.jpg"])
        .   sort
        .   map fileName



    it "orders results" $ \(conn, _) -> do
      desc <- findPictures
        conn
        def { tags = Just ["a"], orderBy = OrderBy FileName DESC }
      asc <- findPictures
        conn
        def { tags = Just ["a"], orderBy = OrderBy FileName ASC }
      (map uuid desc) `shouldBe` (reverse $ map uuid asc)

    it "paginates results" $ \(conn, _) -> do
      findPictures
          conn
          def
            { tags       = Just ["d"]
            , pagination =
              Just (PaginationInput { page = Nothing, pageSize = Just 2 })
            }
        >>= (`shouldBe` 2)
        .   length
      findPictures
          conn
          def
            { tags       = Just ["d"]
            , pagination =
              Just (PaginationInput { page = Just 2, pageSize = Just 2 })
            }
        >>= (`shouldBe` 1)
        .   length
      findPictures
          conn
          def
            { tags       = Just ["d"]
            , pagination =
              Just (PaginationInput { page = Just 3, pageSize = Just 2 })
            }
        >>= (`shouldBe` 0)
        .   length

main :: IO ()
main = hspec $ spec
