module Spec.TestCase
  ( module Test.Hspec
  , bracket
  , evaluate
  , shouldBeRightAnd
  , shouldBeLeftAnd
  )
where

import           Test.Hspec

import           Control.Exception              ( bracket
                                                , evaluate
                                                )

-- Expectations
----------------------------------------------------------------------

shouldBeRightAnd
  :: HasCallStack => Show e => Either e a -> (a -> Bool) -> Expectation
shouldBeRightAnd ea pred = case ea of
  Left  e -> expectationFailure $ "Expected 'Right', got 'Left':\n" <> show e
  Right a -> pred a `shouldBe` True

shouldBeLeftAnd
  :: HasCallStack => Show a => Either e a -> (e -> Bool) -> Expectation
shouldBeLeftAnd ea pred = case ea of
  Left  e -> pred e `shouldBe` True
  Right a -> expectationFailure $ "Expected 'Left', got 'Right':\n" <> show a
