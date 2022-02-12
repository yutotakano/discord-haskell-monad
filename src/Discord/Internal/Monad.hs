{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-} -- allow instance declaration of MonadDiscord
{-# LANGUAGE FlexibleContexts #-} -- allow instance declaration for MonadReader Auth
{-# LANGUAGE UndecidableInstances #-} -- necessary for MonadIO constraint on MonadDiscord, safe because 
{-|
Module      : Discord.Internal.Monad
Description : The DiscordMonad class and its instances.
Copyright   : (c) 2022 Yuto Takano
Maintainer  : moa17stock@gmail.com
License     : MIT (see the LICENSE file)

This module contains the @MonadDiscord@ data class, which abstracts away all
the possible REST interactions with Discord. It also defines instances for
@DiscordHandler@ and @ReaderT Auth IO@. Finally, it declares @RestCallErrorCode@
as an instance of @Exception@, which can be thrown in @MonadDiscord@.

Documentation of each of the functions can be found on the [Discord API Docs]
(https://discord.com/developers/docs/), since the names are not so different.
-}

module Discord.Internal.Monad (MonadDiscord(..)) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (Exception, MonadMask, throwM, throwString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (void)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Ix (inRange)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Network.HTTP.Req as Req

import Discord.Internal.Rest.Prelude
import Discord.Internal.Rest.HTTP
    (RestCallInternalException(..), Request(..), JsonRequest(..))
import qualified Discord.Requests as R
import Discord.Interactions
import Discord.Types
import Discord

instance Exception RestCallErrorCode

-- | @MonadDiscord@ is a data class of Monads that can interact with Discord.
-- It requires MonadMask (which requires MonadThrow and MonadCatch) to get the
-- nice exception things for RestCallErrorCode.
class (Monad m, MonadMask m) => MonadDiscord m where
    {-# MINIMAL processRequest #-}
    processRequest :: (Request (r a), FromJSON a) => r a -> m a
    -- Application Commands
    getGlobalApplicationCommands :: ApplicationId -> m [ApplicationCommand]
    getGlobalApplicationCommands = processRequest . R.GetGlobalApplicationCommands
    createGlobalApplicationCommand :: ApplicationId -> CreateApplicationCommand -> m ApplicationCommand
    createGlobalApplicationCommand = (processRequest .) . R.CreateGlobalApplicationCommand
    getGlobalApplicationCommand :: ApplicationId -> ApplicationCommandId -> m ApplicationCommand
    getGlobalApplicationCommand = (processRequest .) . R.GetGlobalApplicationCommand
    editGlobalApplicationCommand :: ApplicationId -> ApplicationCommandId -> EditApplicationCommand -> m ApplicationCommand
    editGlobalApplicationCommand = ((processRequest .) .) . R.EditGlobalApplicationCommand
    deleteGlobalApplicationCommand :: ApplicationId -> ApplicationCommandId -> m ()
    deleteGlobalApplicationCommand = (processRequest .) . R.DeleteGlobalApplicationCommand
    bulkOverWriteGlobalApplicationCommand :: ApplicationId -> [CreateApplicationCommand] -> m ()
    bulkOverWriteGlobalApplicationCommand = (processRequest .) . R.BulkOverWriteGlobalApplicationCommand
    getGuildApplicationCommands :: ApplicationId -> GuildId -> m [ApplicationCommand]
    getGuildApplicationCommands = (processRequest .) . R.GetGuildApplicationCommands
    createGuildApplicationCommand :: ApplicationId -> GuildId -> CreateApplicationCommand -> m ApplicationCommand
    createGuildApplicationCommand = ((processRequest .) .) . R.CreateGuildApplicationCommand
    getGuildApplicationCommand :: ApplicationId -> GuildId -> ApplicationCommandId -> m ApplicationCommand
    getGuildApplicationCommand = ((processRequest .) .) . R.GetGuildApplicationCommand
    editGuildApplicationCommand :: ApplicationId -> GuildId -> ApplicationCommandId -> CreateApplicationCommand -> m ApplicationCommand
    editGuildApplicationCommand = (((processRequest .) .) .) . R.EditGuildApplicationCommand
    deleteGuildApplicationCommand :: ApplicationId -> GuildId -> ApplicationCommandId -> m ()
    deleteGuildApplicationCommand = ((processRequest .) .) . R.DeleteGuildApplicationCommand
    bulkOverWriteGuildApplicationCommand :: ApplicationId -> GuildId -> [CreateApplicationCommand] -> m ()
    bulkOverWriteGuildApplicationCommand = ((processRequest .) .) . R.BulkOverWriteGuildApplicationCommand
    getGuildApplicationCommandPermissions :: ApplicationId -> GuildId -> m GuildApplicationCommandPermissions
    getGuildApplicationCommandPermissions = (processRequest .) . R.GetGuildApplicationCommandPermissions
    getApplicationCommandPermissions :: ApplicationId -> GuildId -> ApplicationCommandId -> m GuildApplicationCommandPermissions
    getApplicationCommandPermissions = ((processRequest .) .) . R.GetApplicationCommandPermissions
    editApplicationCommandPermissions :: ApplicationId -> GuildId -> ApplicationCommandId -> [ApplicationCommandPermissions] -> m GuildApplicationCommandPermissions
    editApplicationCommandPermissions = (((processRequest .) .) .) . R.EditApplicationCommandPermissions
    batchEditApplicationCommandPermissions :: ApplicationId -> GuildId -> [GuildApplicationCommandPermissions] -> m [GuildApplicationCommandPermissions]
    batchEditApplicationCommandPermissions = ((processRequest .) .) . R.BatchEditApplicationCommandPermissions
    -- Channels
    getChannel :: ChannelId -> m Channel
    getChannel = processRequest . R.GetChannel
    modifyChannel :: ChannelId -> R.ModifyChannelOpts -> m Channel
    modifyChannel = (processRequest .) . R.ModifyChannel
    deleteChannel :: ChannelId -> m Channel
    deleteChannel = processRequest . R.DeleteChannel
    getChannelMessages :: ChannelId -> (Int, R.MessageTiming) -> m [Message]
    getChannelMessages = (processRequest .) . R.GetChannelMessages
    getChannelMessage :: (ChannelId, MessageId) -> m Message
    getChannelMessage = processRequest . R.GetChannelMessage
    createMessage :: ChannelId -> T.Text -> m Message
    createMessage = (processRequest .) . R.CreateMessage
    createMessageUploadFile :: ChannelId -> T.Text -> B.ByteString -> m Message
    createMessageUploadFile = ((processRequest .) .) . R.CreateMessageUploadFile
    createMessageDetailed :: ChannelId -> R.MessageDetailedOpts -> m Message
    createMessageDetailed = (processRequest .) . R.CreateMessageDetailed
    createReaction :: (ChannelId, MessageId) -> T.Text -> m ()
    createReaction = (processRequest .) . R.CreateReaction
    deleteOwnReaction :: (ChannelId, MessageId) -> T.Text -> m ()
    deleteOwnReaction = (processRequest .) . R.DeleteOwnReaction
    deleteUserReaction :: (ChannelId, MessageId) -> UserId -> T.Text -> m ()
    deleteUserReaction = ((processRequest .) .) . R.DeleteUserReaction
    deleteSingleReaction :: (ChannelId, MessageId) -> T.Text -> m ()
    deleteSingleReaction = (processRequest .) . R.DeleteSingleReaction
    getReactions :: (ChannelId, MessageId) -> T.Text -> (Int, R.ReactionTiming) -> m [User]
    getReactions = ((processRequest .) .) . R.GetReactions
    deleteAllReactions :: (ChannelId, MessageId) -> m ()
    deleteAllReactions = processRequest . R.DeleteAllReactions
    editMessage :: (ChannelId, MessageId) -> T.Text -> Maybe CreateEmbed -> m Message
    editMessage = ((processRequest .) .) . R.EditMessage
    deleteMessage :: (ChannelId, MessageId) -> m ()
    deleteMessage = processRequest . R.DeleteMessage
    bulkDeleteMessage :: (ChannelId, [MessageId]) -> m ()
    bulkDeleteMessage = processRequest . R.BulkDeleteMessage
    editChannelPermissions :: ChannelId -> OverwriteId -> R.ChannelPermissionsOpts -> m ()
    editChannelPermissions = ((processRequest .) .) . R.EditChannelPermissions
    getChannelInvites  :: ChannelId -> m Object
    getChannelInvites = processRequest . R.GetChannelInvites
    createChannelInvite :: ChannelId -> R.ChannelInviteOpts -> m Invite
    createChannelInvite = (processRequest .) . R.CreateChannelInvite
    deleteChannelPermission :: ChannelId -> OverwriteId -> m ()
    deleteChannelPermission = (processRequest .) . R.DeleteChannelPermission
    triggerTypingIndicator :: ChannelId -> m ()
    triggerTypingIndicator = processRequest . R.TriggerTypingIndicator
    getPinnedMessages :: ChannelId -> m [Message]
    getPinnedMessages = processRequest . R.GetPinnedMessages
    addPinnedMessage :: (ChannelId, MessageId) -> m ()
    addPinnedMessage = processRequest . R.AddPinnedMessage
    deletePinnedMessage :: (ChannelId, MessageId) -> m ()
    deletePinnedMessage = processRequest . R.DeletePinnedMessage
    groupDMAddRecipient :: ChannelId -> R.GroupDMAddRecipientOpts -> m ()
    groupDMAddRecipient = (processRequest .) . R.GroupDMAddRecipient
    groupDMRemoveRecipient :: ChannelId -> UserId -> m ()
    groupDMRemoveRecipient = (processRequest .) . R.GroupDMRemoveRecipient
    -- Emojis
    listGuildEmojis :: GuildId -> m [Emoji]
    listGuildEmojis = processRequest . R.ListGuildEmojis
    getGuildEmoji :: GuildId -> EmojiId -> m Emoji
    getGuildEmoji = (processRequest .) . R.GetGuildEmoji
    createGuildEmoji :: GuildId -> T.Text -> B.ByteString -> m Emoji
    createGuildEmoji g t b  = case R.parseEmojiImage b of
        Left  x -> throwString $ T.unpack x
        Right x -> processRequest (R.CreateGuildEmoji g t x)
    modifyGuildEmoji :: GuildId -> EmojiId -> R.ModifyGuildEmojiOpts -> m Emoji
    modifyGuildEmoji = ((processRequest .) .) . R.ModifyGuildEmoji
    deleteGuildEmoji :: GuildId -> EmojiId -> m ()
    deleteGuildEmoji = (processRequest .) . R.DeleteGuildEmoji
    -- Guilds
    getGuild :: GuildId -> m Guild
    getGuild = processRequest . R.GetGuild
    modifyGuild :: GuildId -> R.ModifyGuildOpts -> m Guild
    modifyGuild = (processRequest .) . R.ModifyGuild
    deleteGuild :: GuildId -> m ()
    deleteGuild = processRequest . R.DeleteGuild
    getGuildChannels :: GuildId -> m [Channel]
    getGuildChannels = processRequest . R.GetGuildChannels
    createGuildChannel :: GuildId -> T.Text -> [Overwrite] -> R.CreateGuildChannelOpts -> m Channel
    createGuildChannel = (((processRequest .) .) .) . R.CreateGuildChannel
    modifyGuildChannelPositions :: GuildId -> [(ChannelId, Int)] -> m [Channel]
    modifyGuildChannelPositions = (processRequest .) . R.ModifyGuildChannelPositions
    getGuildMember :: GuildId -> UserId -> m GuildMember
    getGuildMember = (processRequest .) . R.GetGuildMember
    listGuildMembers :: GuildId -> R.GuildMembersTiming -> m [GuildMember]
    listGuildMembers = (processRequest .) . R.ListGuildMembers
    addGuildMember :: GuildId -> UserId -> R.AddGuildMemberOpts -> m ()
    addGuildMember = ((processRequest .) .) . R.AddGuildMember
    modifyGuildMember :: GuildId -> UserId -> R.ModifyGuildMemberOpts -> m ()
    modifyGuildMember = ((processRequest .) .) . R.ModifyGuildMember
    modifyCurrentUserNick :: GuildId -> T.Text -> m ()
    modifyCurrentUserNick = (processRequest .) . R.ModifyCurrentUserNick
    addGuildMemberRole :: GuildId -> UserId -> RoleId -> m ()
    addGuildMemberRole = ((processRequest .) .) . R.AddGuildMemberRole
    removeGuildMemberRole :: GuildId -> UserId -> RoleId -> m ()
    removeGuildMemberRole = ((processRequest .) .) . R.RemoveGuildMemberRole
    removeGuildMember :: GuildId -> UserId -> m ()
    removeGuildMember = (processRequest .) . R.RemoveGuildMember
    getGuildBans :: GuildId -> m [GuildBan]
    getGuildBans = processRequest . R.GetGuildBans
    getGuildBan :: GuildId -> UserId -> m GuildBan
    getGuildBan = (processRequest .) . R.GetGuildBan
    createGuildBan :: GuildId -> UserId -> R.CreateGuildBanOpts -> m ()
    createGuildBan = ((processRequest .) .) . R.CreateGuildBan
    removeGuildBan :: GuildId -> UserId -> m ()
    removeGuildBan = (processRequest .) . R.RemoveGuildBan
    getGuildRoles :: GuildId -> m [Role]
    getGuildRoles = processRequest . R.GetGuildRoles
    createGuildRole :: GuildId -> R.ModifyGuildRoleOpts -> m Role
    createGuildRole = (processRequest .) . R.CreateGuildRole
    modifyGuildRolePositions :: GuildId -> [(RoleId, Integer)] -> m [Role]
    modifyGuildRolePositions = (processRequest .) . R.ModifyGuildRolePositions
    modifyGuildRole :: GuildId -> RoleId -> R.ModifyGuildRoleOpts -> m Role
    modifyGuildRole = ((processRequest .) .) . R.ModifyGuildRole
    deleteGuildRole :: GuildId -> RoleId -> m ()
    deleteGuildRole = (processRequest .) . R.DeleteGuildRole
    getGuildPruneCount :: GuildId -> Integer -> m Object
    getGuildPruneCount = (processRequest .) . R.GetGuildPruneCount
    beginGuildPrune :: GuildId -> Integer -> m Object
    beginGuildPrune = (processRequest .) . R.BeginGuildPrune
    getGuildVoiceRegions :: GuildId -> m [VoiceRegion]
    getGuildVoiceRegions = processRequest . R.GetGuildVoiceRegions
    getGuildInvites :: GuildId -> m [Invite]
    getGuildInvites = processRequest . R.GetGuildInvites
    getGuildIntegrations :: GuildId -> m [Integration]
    getGuildIntegrations = processRequest . R.GetGuildIntegrations
    createGuildIntegration :: GuildId -> IntegrationId -> R.CreateGuildIntegrationOpts -> m ()
    createGuildIntegration = ((processRequest .) .) . R.CreateGuildIntegration
    modifyGuildIntegration :: GuildId -> IntegrationId -> R.ModifyGuildIntegrationOpts -> m ()
    modifyGuildIntegration = ((processRequest .) .) . R.ModifyGuildIntegration
    deleteGuildIntegration :: GuildId -> IntegrationId -> m ()
    deleteGuildIntegration = (processRequest .) . R.DeleteGuildIntegration
    syncGuildIntegration :: GuildId -> IntegrationId -> m ()
    syncGuildIntegration = (processRequest .) . R.SyncGuildIntegration
    getGuildWidget :: GuildId -> m GuildWidget
    getGuildWidget = processRequest . R.GetGuildWidget
    modifyGuildWidget :: GuildId -> GuildWidget -> m GuildWidget
    modifyGuildWidget = (processRequest .) . R.ModifyGuildWidget
    getGuildVanityURL :: GuildId -> m T.Text
    getGuildVanityURL = processRequest . R.GetGuildVanityURL
    -- Interactions
    createInteractionResponse :: InteractionId -> InteractionToken -> InteractionResponse -> m ()
    createInteractionResponse = ((processRequest .) .) . R.CreateInteractionResponse
    getOriginalInteractionResponse :: ApplicationId -> InteractionToken -> m Message
    getOriginalInteractionResponse = (processRequest .) . R.GetOriginalInteractionResponse
    editOriginalInteractionResponse :: ApplicationId -> InteractionToken -> InteractionResponseMessage -> m Message
    editOriginalInteractionResponse = ((processRequest .) .) . R.EditOriginalInteractionResponse
    deleteOriginalInteractionResponse :: ApplicationId -> InteractionToken -> m ()
    deleteOriginalInteractionResponse = (processRequest .) . R.DeleteOriginalInteractionResponse
    createFollowupInteractionMessage :: ApplicationId -> InteractionToken -> InteractionResponseMessage -> m Message
    createFollowupInteractionMessage = ((processRequest .) .) . R.CreateFollowupInteractionMessage
    getFollowupInteractionMessage :: ApplicationId -> InteractionToken -> MessageId -> m Message
    getFollowupInteractionMessage = ((processRequest .) .) . R.GetFollowupInteractionMessage
    editFollowupInteractionMessage :: ApplicationId -> InteractionToken -> MessageId -> InteractionResponse -> m Message
    editFollowupInteractionMessage = (((processRequest .) .) .) . R.EditFollowupInteractionMessage
    deleteFollowupInteractionMessage :: ApplicationId -> InteractionToken -> MessageId -> m ()
    deleteFollowupInteractionMessage = ((processRequest .) .) . R.DeleteFollowupInteractionMessage
    -- Invites
    getInvite :: T.Text -> m Invite
    getInvite = processRequest . R.GetInvite
    deleteInvite :: T.Text -> m Invite
    deleteInvite = processRequest . R.DeleteInvite
    -- Users
    getCurrentUser :: m User
    getCurrentUser = processRequest R.GetCurrentUser
    getUser :: UserId -> m User
    getUser = processRequest . R.GetUser
    modifyCurrentUser :: T.Text -> R.CurrentUserAvatar -> m User
    modifyCurrentUser = (processRequest .) . R.ModifyCurrentUser
    getCurrentUserGuilds :: m [PartialGuild]
    getCurrentUserGuilds = processRequest R.GetCurrentUserGuilds
    leaveGuild :: GuildId -> m ()
    leaveGuild = processRequest . R.LeaveGuild
    getUserDMs :: m [Channel]
    getUserDMs = processRequest R.GetUserDMs
    createDM :: UserId -> m Channel
    createDM = processRequest . R.CreateDM
    getUserConnections :: m [ConnectionObject]
    getUserConnections = processRequest R.GetUserConnections
    -- Voice
    listVoiceRegions :: m [VoiceRegion]
    listVoiceRegions = processRequest R.ListVoiceRegions
    -- Webhooks
    createWebhook :: ChannelId -> R.CreateWebhookOpts -> m Webhook
    createWebhook = (processRequest .) . R.CreateWebhook
    getChannelWebhooks :: ChannelId -> m [Webhook]
    getChannelWebhooks = processRequest . R.GetChannelWebhooks
    getGuildWebhooks :: GuildId -> m [Webhook]
    getGuildWebhooks = processRequest . R.GetGuildWebhooks
    getWebhook :: WebhookId -> m Webhook
    getWebhook = processRequest . R.GetWebhook
    getWebhookWithToken :: WebhookId -> T.Text -> m Webhook
    getWebhookWithToken = (processRequest .) . R.GetWebhookWithToken
    modifyWebhook :: WebhookId -> R.ModifyWebhookOpts -> m Webhook
    modifyWebhook = (processRequest .) . R.ModifyWebhook
    modifyWebhookWithToken :: WebhookId -> T.Text -> R.ModifyWebhookOpts -> m Webhook
    modifyWebhookWithToken = ((processRequest .) .) . R.ModifyWebhookWithToken
    deleteWebhook :: WebhookId -> m ()
    deleteWebhook = processRequest . R.DeleteWebhook
    deleteWebhookWithToken :: WebhookId -> T.Text -> m ()
    deleteWebhookWithToken = (processRequest .) . R.DeleteWebhookWithToken
    executeWebhookWithToken :: WebhookId -> T.Text -> R.ExecuteWebhookWithTokenOpts -> m ()
    executeWebhookWithToken = ((processRequest .) .) . R.ExecuteWebhookWithToken

    -- Custom utilities
    respond :: Message -> T.Text -> m ()
    respond m t = void $ createMessage (messageChannelId m) t

-- | Implements every single possible rest call as a wrapper function.
-- Convenient notation achieved using a point-free restCallAndHandle
instance MonadDiscord DiscordHandler where
    processRequest = restCallAndHandle

-- | @restCallAndHandle@ calls a request and returns it in the DiscordHandler
-- monad, throwing @RestCallErrorCode@ on errors.
restCallAndHandle :: (Request (r a), FromJSON a) => r a -> DiscordHandler a
restCallAndHandle req = restCall req >>= handleDiscordResult

-- | Handles the response of discord-haskell's REST calls in DiscordHandler
-- and throws a 'RestCallErrorCode' if the call errored.
handleDiscordResult :: Either RestCallErrorCode a -> DiscordHandler a
handleDiscordResult result = case result of
    Left  e -> throwM e
    Right x -> pure x

-- | Any ReaderT monad with the Auth accessible is a valid Discord monad.
--
-- Usage:
--
-- @
-- import Control.Monad.Reader (runReaderT)
-- import Control.Monad (void)
-- someFunc :: IO ()
-- someFunc = runReaderT (void $ createMessage 1234 "text") (Auth "tokenhere")
-- @
instance (MonadIO m, MonadMask m) => MonadDiscord (ReaderT Auth m) where
    processRequest = callRestIO

-- the following is a GIGANTIC mess basically cut and pasted from discord-haskell's
-- internals and simplified, because it's alas, not exported.

-- | Do an IO call to get the result of the request, and throw errors
-- whenever some error occurs. A simplified version of the logic within
-- discord-haskell's Discord.Internal.Rest.HTTP module, as it doesn't need to
-- store the error information, it can just throw it.
callRestIO :: (MonadIO m, MonadMask m, MonadReader Auth m, Request (r a), FromJSON a) => r a -> m a
callRestIO req = do
    -- get the token
    auth <- ask
    -- create a request with the auth in the header
    let action = compileRequest auth (jsonRequest req)
    -- send off the request, get the response
    resp <- liftIO $ restIOtoIO action
    let body   = Req.responseBody resp
        code   = Req.responseStatusCode resp
        status = TE.decodeUtf8 $ Req.responseStatusMessage resp
    case () of
        _ | code `elem` [429, 500, 502] -> do
            -- wait a bit before retrying.
            liftIO $ threadDelay (10 * 10 ^ (6 :: Int))
            callRestIO req
        _ | code >= 200 && code <= 299 -> do
            let parsableBody = if body == "" then "[]" else body
            case eitherDecode parsableBody of
                Right o  -> pure o
                Left  er -> do
                    let formaterr = T.pack $ "Response could not be parsed " <> er
                    throwM $ RestCallErrorCode code "err" formaterr
        _ | otherwise -> do
            throwM $ RestCallErrorCode
                code
                status
                (TE.decodeUtf8 $ BL.toStrict body)

-- | From a JsonRequest, create a RestIO thingy with the appropriately auth
-- headers. Idk why this is necessary but it doesn't work without it.
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

