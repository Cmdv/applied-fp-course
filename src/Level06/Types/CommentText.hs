{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Level06.Types.CommentText
  ( CommentText
  , mkCommentText
  , getCommentText
  ) where

import Level06.Types.Error (Error(EmptyCommentText), nonEmptyText)

import Data.Text (Text)
import Data.Aeson (ToJSON)

newtype CommentText = CommentText Text
  deriving (Show, ToJSON)

mkCommentText
  :: Text
  -> Either Error CommentText
mkCommentText =
  nonEmptyText CommentText EmptyCommentText

getCommentText
  :: CommentText
  -> Text
getCommentText (CommentText t) =
  t
