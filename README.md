# discord-haskell-monad

This is a supplementary library to `discord-haskell` that provides an
exception-oriented abstraction layer for Discord API REST calls. It bases its
existence on the idea that Either should be reserved for pure code, while
Exceptions should be used for IO due to its complex unpredictability. This idea
comes from various sources including the
[Exceptions Best Practices](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/)
post by Michael Snoyman and the
[Learn Haskell by Building a Blog Generator](https://lhbg-book.link/06-errors_and_files/05-summary.html)
book by Gil Mizrahi.

Reasoning about errors in IO with Either, as `discord-haskell` currently does
with `RestCallErrorCode`, is unreliable and leads to unnecessarily messy user
code. The line grows blurry between what is handled by the Either and what is not.
For instance, while e.g. HTTP 400 errors are returned by `restCall` through
`Left RestCallErrorCode`, connection timeout exceptions are not. Many are logged
to `discord-haskell`'s internal log. I wondered whether those HTTP 400 errors
truly needed to be handled with Either anyway.

The library, attempting to be an answer to that question, introduces a
`MonadDiscord` type-class. It has a single method: `call`. This is a sort-of
replacement of `restCall`, but throws HTTP error codes, network errors, and
parse errors as Exceptions in IO.

```hs
example :: (MonadDiscord m) => m ()
example = do
    m <- call (R.CreateMessage 123123123 "Hello World!")
-- `catches` [Handler (\ (ex :: RestCallErrorCode)      -> handleAPIError ex),
--            Handler (\ (ex :: ResponseParseException) -> handleParseError ex),
--            Handler (\ (ex :: HttpException)          -> handleHttpError ex)]
    print $ messageId m
```

The library also has two instances of this monad defined, one for
`DiscordHandler` and one for IO environments with access an auth token.

## The DiscordHandler instance

The DiscordHandler instance of `DiscordMonad` solves the indentation problem
that is present with the `Either RestCallErrorCode a` type that `discord-haskell`
uses for its calls. For example, the following is what you might typically write
with `discord-haskell`.

```hs
-- Send a message and react on it
example :: DiscordHandler ()
example = do
  eMsg <- restCall $ R.CreateMessage 123412341234 "Hello World!"
  case eMsg of
    Left e -> liftIO (print "first msg failed") >> pure ()
    Right msg -> do
      result <- restCall $ R.CreateReaction (messageChannel msg, messageId msg) "eyes"
      case result of
        Left e -> liftIO (print "reaction failed") >> pure ()
        Right _ -> liftIO $ putStrLn "All good!"
```

You could use the EitherT transformer, but if rollback is required for certain
actions it is difficult. The code is also more difficult to understand since you
have to sprinkle `lift`s throughout.

The following is what you would replace the above by, using this library.

```hs
import Discord.Monad
--- Send a message and react on it
example :: DiscordHandler ()
example = do
  msg <- call $ R.CreateMessage 123412341234 "Hello World!"
  call $ R.CreateReaction (messageChannel msg, messageId msg) "eyes"
  liftIO $ putStrLn "All good!"
```

The `RestCallErrorCode` that was previously in `Left` is now an Exception.
This means that you can use `handle`, `catch`, `finally`, etc (from the
`safe-exceptions` package) wherever.

In addition, the code may now explicitly throw a `HttpException` or a
`ResponseParseException`, which were previously consumed after logging. Note that,
due to this separation of errors and explicit exception throws, the functionality
you get from `try . call $ ... :: Either RestCallErrorCode a` is different to
the original `restCall`. Specifically, all parse errors which would have
originally been returned as ErrorCode 400 will not be caught by that `try`, nor
will HttpExceptions, which would originally have caused an automatic retry of
the request.

## The MonadReader Auth instance

This library also provides an instance of `MonadDiscord` for all Monads `m` that
satisfy the constraint `(MonadIO m, MonadReader Auth m, MonadMask m)`.

In plain English, this means any IO monad with access to the Auth can send
Discord requests (complete with rate-limiting). This does not require running
the Bot's Rest and Event loop, reducing the resources needed to send a simple
API request.

```hs
main :: IO Message
main = runReaderT (call $ R.CreateMessage 123412341234 "Hello World!") (Auth "token here")
```

## Concluding Notes

Please feel free to contribute through issues or pull requests if you want to add anything!

This library was developed while developing a [Discord bot for our university server](https://github.com/yellowtides/owenbot-hs).
