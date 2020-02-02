module Model.PaginationSpec where

import           Spec.TestCase

import           Model.Pagination

-- Tests
----------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Model.Pagination" $ do
    describe "parsePaginationInput" $ do
      it "returns PaginationParams from PaginationInput" $ do
        parsePaginationInput
            (Just PaginationInput { page = Just 1, pageSize = Just 10 })
          `shouldBe` PaginationParams { offset = 0, limit = 10 }
        parsePaginationInput
            (Just PaginationInput { page = Just 2, pageSize = Just 10 })
          `shouldBe` PaginationParams { offset = 10, limit = 10 }
        parsePaginationInput
            (Just PaginationInput { page = Just 3, pageSize = Just 10 })
          `shouldBe` PaginationParams { offset = 20, limit = 10 }

      it "applies default values to Nothing" $ do
        parsePaginationInput
            (Just PaginationInput { page = Nothing, pageSize = Nothing })
          `shouldBe` PaginationParams { offset = 0, limit = 100 }
        parsePaginationInput Nothing
          `shouldBe` PaginationParams { offset = 0, limit = 100 }

      it "limits PaginationInput" $ do
        parsePaginationInput
            (Just PaginationInput { page = Just (-1), pageSize = Just 10 })
          `shouldBe` PaginationParams { offset = 0, limit = 10 }
        parsePaginationInput
            (Just PaginationInput { page = Just 1, pageSize = Just (-1) })
          `shouldBe` PaginationParams { offset = 0, limit = 1 }

main :: IO ()
main = hspec $ spec
