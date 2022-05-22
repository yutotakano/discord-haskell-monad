{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-} -- allow instance declaration of MonadDiscord
{-# LANGUAGE FlexibleContexts #-} -- allow instance declaration for MonadReader Auth
{-# LANGUAGE UndecidableInstances #-} -- necessary for MonadIO constraint on MonadDiscord
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Discord.Internal.Monad
Description : The DiscordMonad class and its instances.
Copyright   : (c) 2022 Yuto Takano
Maintainer  : moa17stock@gmail.com
License     : MIT (see the LICENSE file)

This module contains the 'MonadDiscord' type class to call Discord Requests.
An instance for 'DiscordHandler' and all monads that satisfy @MonadReader Auth@
are provided.

Based on the idea that Either should be used only for pure code and that
Exceptions should be used for unpredictable IO code, this library puts a thin
layer on top of @discord-haskell@ so that all types of errors are thrown as
exceptions. This includes: 'RestCallErrorCode', 'Req.HttpException', and
'ResponseParseException'.

A bit of an example code of what this means:

@@
example :: (MonadDiscord m) => m ()
example = do
    m <- call (R.CreateMessage 123123123 "Hello World!")
-- `catches` [Handler (\ (ex :: RestCallErrorCode)      -> handleAPIError ex),
--            Handler (\ (ex :: ResponseParseException) -> handleParseError ex),
--            Handler (\ (ex :: HttpException)          -> handleHttpError ex)]
    print $ messageId m
@@

Please see the documentation below, as well as the README on GitHub for
explanations.
-}

module Discord.Internal.Monad where

import Control.Concurrent (threadDelay, isEmptyMVar)
import Control.Exception.Safe (Exception, MonadMask, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import qualified Data.Aeson as A (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Network.HTTP.Req as Req

import Discord.Internal.Rest (writeRestCall)
import Discord.Internal.Rest.Prelude
import Discord.Internal.Rest.HTTP
    (RestCallInternalException(..), Request(..), JsonRequest(..))
import Discord.Handle (DiscordHandle(..))
import Discord.Types
import Discord

-- | We redefine the 'RestCallErrorCode' as an IO Exception that is thrown when
-- the Discord API returns an unexpected response code.
instance Exception RestCallErrorCode

-- | We introduce a new datatype for parse errors. This replaces discord-haskell's
-- default behaviour which is to print the error to the log and return a 400
-- rest error code.
data ResponseParseException = ResponseParseException String BL.ByteString
  deriving Show
instance Exception ResponseParseException

-- | @MonadDiscord@ is a class of Monads that can invoke a Discord
-- 'Discord.Internal.Rest.Prelude.Request'.
class (Monad m, MonadMask m) => MonadDiscord m where
    {-# MINIMAL call #-}
    -- | Calls the Discord API with the specific Request.
    -- This may throw the following:
    --
    -- * 'RestCallErrorCode' on some Discord error
    -- * 'Req.HttpException' on HTTP error
    -- * 'ResponseParseException' on a parse error
    --
    -- In addition, any instances of MonadDiscord should obey and throw only
    -- exceptions of the above type in appropriate cases.
    call :: (Request (r a), A.FromJSON a) => r a -> m a

-- | 'DiscordHandler' is trivially a Discord monad.
-- We recreate the implementation of 'restCall' but slightly altered to rethrow
-- exceptions in the style we want.
--
-- See the implementation of restCall here:
-- https://github.com/aquarial/discord-haskell/blob/c009edc5a5cd4991f600315686d5a1c880ea683a/src/Discord.hs#L128
instance {-# OVERLAPPING #-} MonadDiscord DiscordHandler where
    call req = do
        h <- ask
        empty <- liftIO $ isEmptyMVar (discordHandleLibraryError h)
        if not empty then
            throwIO $ RestCallErrorCode 400 "Library Stopped Working" ""
        else do
            -- writeRestCall and discord-haskell's internals handle rate limits
            -- and retry automatically.
            resp <- liftIO $ writeRestCall (discordHandleRestChan h) req
            case resp of
                Right x -> pure x
                Left (RestCallInternalErrorCode c e1 e2) ->
                    throwIO $ RestCallErrorCode c (TE.decodeUtf8 e1) (TE.decodeUtf8 e2)
                Left (RestCallInternalHttpException e1) ->
                    -- in discord-haskell's restCall, the request is retried here
                    -- automatically after a delay. We choose not to do that and
                    -- rethrow the HttpException.
                    throwIO e1
                Left (RestCallInternalNoParse err dat) ->
                    throwIO $ ResponseParseException err dat

-- | Any Reader monad with the 'Auth' accessible is a valid Discord monad.
--
-- Usage:
--
-- @
-- import Control.Monad.Reader (runReaderT)
-- someFunc :: IO Message
-- someFunc = runReaderT (call $ CreateMessage 1234 "text") (Auth "tokenhere")
-- @
instance (Monad m, MonadIO m, MonadMask m, MonadReader Auth m) => MonadDiscord m where
    -- The following is an adapted and simplified version of the logic within
    -- discord-haskell's @Discord.Internal.Rest.HTTP@ module. It does not handle
    -- HTTPExceptions, and also throws all RestCallErrorCodes immediately upon
    -- receiving. The function will retry if a rate limit is encountered.
    call req = do
        -- get the token
        auth <- ask
        -- create a request with the auth in the header
        let action = compileRequest auth (jsonRequest req)
        -- send off the request, get the response. May throw HttpException.
        resp <- liftIO $ restIOtoIO action
        let body = Req.responseBody resp
            code = Req.responseStatusCode resp
            status = TE.decodeUtf8 $ Req.responseStatusMessage resp
        case () of
            _ | code `elem` [429, 500, 502] -> do
                -- wait a bit before retrying.
                liftIO $ threadDelay (10 * 10 ^ (6 :: Int))
                call req
            _ | code >= 200 && code <= 299 -> do
                let parsableBody = if body == "" then "[]" else body
                case A.eitherDecode parsableBody of
                    Right o -> pure o
                    Left er -> throwIO $ ResponseParseException er body
            _ | otherwise -> do
                throwIO $ RestCallErrorCode
                    code
                    status
                    (TE.decodeUtf8 $ BL.toStrict body)

-- | From a 'JsonRequest', create a RestIO thingy with the appropriately auth
-- headers.
compileRequest :: Auth -> JsonRequest -> RestIO Req.LbsResponse
compileRequest auth request =
    let authopt = authHeader auth <> Req.header "X-RateLimit-Precision" "millisecond"
    in
        case request of
            (Delete url opts) ->
                Req.req Req.DELETE url Req.NoReqBody Req.lbsResponse (authopt <> opts)
            (Get url opts) ->
                Req.req Req.GET url Req.NoReqBody Req.lbsResponse (authopt <> opts)
            (Put url body opts) ->
                Req.req Req.PUT url body Req.lbsResponse (authopt <> opts)
            (Patch url body opts) -> do
                b <- body
                Req.req Req.PATCH url b Req.lbsResponse (authopt <> opts)
            (Post url body opts) -> do
                b <- body
                Req.req Req.POST url b Req.lbsResponse (authopt <> opts)

