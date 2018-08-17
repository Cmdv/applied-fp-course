{-# LANGUAGE OverloadedStrings #-}
module Level04Tests
  ( unitTests
  , doctests
  ) where

import           Control.Monad  (join)

import           Test.Hspec
import           Test.Hspec.Wai

import qualified System.Exit    as Exit

import qualified Level04.Core   as Core
import qualified Level04.DB     as DB
import qualified Level04.Types  as Types

-- Don't forget to uncomment these functions in @tests/Test.hs@ otherwise your
-- tests won't be run.

doctests :: [FilePath]
doctests =
  [ "-isrc"
  , "src/Level04/Conf.hs"
  , "src/Level04/DB.hs"
  , "src/Level04/Types.hs"
  ]

unitTests :: IO ()
unitTests = do
  let dieWith m = print m >> Exit.exitFailure

  reqsE <- Core.prepareAppReqs
  case reqsE of

    Left err -> dieWith err

    Right db -> do
      let app' = pure ( Core.app db )

          flushTopic =
            -- Clean up and yell about our errors
            fmap ( either dieWith pure . join ) .
            -- Purge all of the comments for this topic for our tests
            traverse ( DB.deleteTopic db )
            -- We don't export the constructor so even for known values we have
            -- to play by the rules. There is no - "Oh just this one time.", do it right.
            $ Types.mkTopic "fudge"

      -- Run the tests with a DB topic flush between each spec
      hspec . with ( flushTopic >> app' ) $ do

        -- AddRq Spec
        describe "POST /topic/add" $ do

          it "Should return 200 with well formed request" $
            post "/fudge/add" "Fred" `shouldRespondWith` "Success"

          it "Should 400 on empty input" $
            post "/fudge/add" "" `shouldRespondWith` 400

        -- ViewRq Spec
        describe "GET /topic/view" $
          it "Should return 200 with content" $ do
            post "/fudge/add" "Is super tasty."
            get "/fudge/view" `shouldRespondWith` 200

        -- ListRq Spec
        describe "GET /list" $
          it "Should return 200 with content" $ do
            post "/fudge/add" "Is super tasty."
            get "/list" `shouldRespondWith` "[\"fudge\"]"
