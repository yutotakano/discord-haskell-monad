# Changelog

## 1.2.1

- Allow `discord-haskell` up to 1.14.0 using CPP conditionals

## 1.2.0

- Breaking: Remove all functions for Rest commands as part of re-assessing the raison d'être of the library. The minimal and only definition is `call`, replacing `processRequest`.
- Throw `HttpException` properly on `HttpException`s (and not `RestCallErrorCode`).
- Throw `ResponseParseException` on parse errors instead of `RestCallErrorCode`.
- Relax constraints on the Reader instance, to `MonadIO m, MonadReader Auth m => MonadDiscord m`.
- Allow `discord-haskell` up to 1.13.0

## 1.1.0

- Bump Stack LTS to 18.24
- Bump `discord-haskell` to 1.12.0
- Add functions in `DiscordMonad` for Interactions and some changes with GuildWidget
- Introduce `processRequest` as a minimal definition for the `DiscordMonad` class, making ReaderT and DiscordHandler instances simpler.

## 1.0.0

- Initial release.
- `DiscordMonad` typeclass with all possible Rest commands defined as functions.
- `MonadIO m => ReaderT Auth m` and `DiscordHandler` instances for `DiscordMonad`.
