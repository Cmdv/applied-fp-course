{-# LANGUAGE OverloadedStrings #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.Text (Text)
import qualified Data.Text as Text

import           Data.Bifunctor (first)
import           Data.Time (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple as Sql

import qualified Database.SQLite.SimpleErrors as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Level05.AppM (AppM, liftEither)

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB
  :: (a -> Either Error b)
  -> IO a
  -> AppM b
runDB f a = do
  r <- liftIO $ first DBError <$> Sql.runDBAction a
  liftEither $ f =<< r

getComments
  :: FirstAppDB
  -> Topic
  -> AppM [Comment]
getComments db t = do
  let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  runDB (traverse fromDBComment) $ Sql.query (dbConn db) q (Sql.Only . getTopic $ t)


addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> AppM ()
addCommentToTopic db t c = do
  nowish <- liftIO getCurrentTime
  let q = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  runDB Right $ Sql.execute (dbConn db) q (getTopic t, getCommentText c, nowish)

getTopics
  :: FirstAppDB
  -> AppM [Topic]
getTopics db =
  let q = "SELECT DISTINCT topic FROM comments"
  in
    runDB (traverse ( mkTopic . Sql.fromOnly )) $ Sql.query_ (dbConn db) q


deleteTopic
  :: FirstAppDB
  -> Topic
  -> AppM ()
deleteTopic db topic =
  let q = "DELETE FROM comments WHERE topic = ?"
  in
    runDB Right $ Sql.execute (dbConn db) q (Sql.Only . getTopic $ topic)
