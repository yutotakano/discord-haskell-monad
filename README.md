# discord-haskell-monad

This is a small extra library around discord-haskell that provides an
abstraction layer for the REST calls to Discord. It does this by defining a
`MonadDiscord` type-class, and making `DiscordHandler` an instance of this.

This enables bot developers to write much simpler code with simpler type
signatures.

## The DiscordHandler instance

With the `Either RestCallErrorCode a` type that `discord-haskell` uses for its
return values of REST calls, handling errors with case statements like below can
get very cumbersome and indent-prone.

```hs
-- Send a message and react on it
example :: DiscordHandler ()
example = do
  eMsg <- restCall $ R.CreateMessage 123412341234 "Hello World!"
  case eMsg of
    Left e -> pure ()
    Right msg -> do
      result <- restCall $ R.CreateReaction (messageChannel msg, messageId msg) "eyes"
      case result of
        Left e -> pure ()
        Right _ -> liftIO $ putStrLn "All good!"
```

You could use the Either monad, but that's a little difficult to understand
notationally.

The example below shows what this library enables you to write.

```hs
import Discord.Monad
--- Send a message and react on it
example :: DiscordHandler ()
example = do
  msg <- createMessage 123412341234 "Hello World!"
  createReaction (messageChannel msg, messageId msg) "eyes"
  liftIO $ putStrLn "All good!"
```

The `RestCallErrorCode` that was previously in `Left` (from `Either`) is now
encompassed as an Exception that can be thrown and caught safely. This means
that you can use `handle`, `catch`, `finally`, etc wherever in the code to
appropriately catch the error. One interesting example is `try`, which when used,
will make each function's types `Either RestCallErrorCode a` -- this is exactly
what the default discord-haskell library provides!

## The ReaderT Auth instance

In addition to the above convenient instance for `DiscordHandler`, this library
provides an instance of `MonadDiscord` for `ReaderT Auth m` where m has the
capabilities of `(MonadIO m, MonadMask m)`.

This has various thankful consequences. The most important and biggest impact is
that calls to the Discord REST API is as intuitive as it can get, even without
running a bot at all. Here is a snippet of its potential use.

```hs
main :: IO Message
main = runReaderT (createMessage 123412341234 "Hello World!") (Auth "token here")
```

## Contribution

Contributions are welcome. I've tried and failed halfway when considering how to
make MonadDiscord fully mtl-style (also, I didn't need those instances).

This library was developed while developing a [Discord bot for our university server](https://github.com/yellowtides/owenbot-hs).
