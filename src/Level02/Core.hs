{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, Method, hContentType, status200,
                                           status400, status404, methodPost, methodGet)

import qualified Data.ByteString.Lazy as LBS

import           Data.Either (either)

import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)

import           Level02.Types           (ContentType(..), Error(..), RqType(..),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to Level02.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse :: Status -> ContentType -> LBS.ByteString -> Response
mkResponse st ct = responseLBS st [("Content-Type", renderContentType ct)]

resp200 :: ContentType -> LBS.ByteString -> Response
resp200 ct = responseLBS status200 [("Content-Type", renderContentType ct)]

resp404 :: ContentType -> LBS.ByteString -> Response
resp404 ct = responseLBS status404 [("Content-Type", renderContentType ct)]

resp400 :: ContentType -> LBS.ByteString -> Response
resp400 ct = responseLBS status400 [("Content-Type", renderContentType ct)]

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest :: Text -> LBS.ByteString -> Either Error RqType
mkAddRequest t b =
  AddRq <$> mkTopic t <*> mkCommentText (lazyByteStringToStrictText b)
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest :: Text -> Either Error RqType
mkViewRequest t =
  ViewRq <$> mkTopic t

mkListRequest :: Either Error RqType
mkListRequest = Right ListRq

mkErrorResponse :: Error -> Response
mkErrorResponse e =
  case e of
    EmptyText -> resp400 PlainText "Bad Request"
    NotFound  -> resp404 PlainText "Not Found"

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest :: Request -> IO ( Either Error RqType )
mkRequest r =
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  case (requestMethod r, pathInfo r) of
    ("POST", [t, "add"]) -> mkAddRequest t <$> strictRequestBody r
    ("GET", [t, "view"]) -> pure $ mkViewRequest t
    ("GET", ["list"])    -> pure mkListRequest
    _ -> pure (Left NotFound)

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest :: RqType -> Either Error Response
handleRequest r = case r of
  AddRq _ _ -> Right $ resp200 PlainText "Not implemented yet"
  ViewRq _  -> Right $ resp200 PlainText "Not implemented yet"
  ListRq    -> Right $ resp200 PlainText "Not implemented yet"

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app :: Application
app request respond = do
  req <- mkRequest request
  let response = case req of
        Left err -> mkErrorResponse err
        Right rq -> case handleRequest rq of
                        Left e -> mkErrorResponse e
                        Right r -> r
  respond response

runApp :: IO ()
runApp = run 3000 app
