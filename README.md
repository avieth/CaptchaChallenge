CaptchaChallenge
================

A (hopefully) reusable utility for doing captcha challenges. Here's how it
works:

Create a `CaptchaChallenge` value by giving information about your Redis
instance.

```Haskell
-- defaultConnectInfo comes from the hedis package.
myCaptchaChallenge :: CaptchaChallenge
myCaptchaChallenge = captchaChallenge defaultConnectInfo
```

Generate a challenge, along with a key by which we can reference it.

```Haskell
...
(key, captcha) <- challenge myCaptchaChallenge
-- key is a base64-encoded ByteString, and captcha is a ByteString to be
-- interpreted as a PNG.
-- Maybe send the key and captcha to a client, and await a response from them
-- with the same key, and proposed solution.
...
```

To attempt a solution, supply the key and a proposed solution:

```Haskell
outcome <- solve myCaptchaChallenge (key, solution)
case outcome of
  Passed p -> ...
  -- ^ Solution matches the one we have on file. p has type ChallengePassed;
  --   you can use that type to indicate statically that your function needs
  --   a captcha challenge before its used.
  Failed -> ...
  -- ^ Solution is no good; are you a robot?
  BadKey -> ...
  -- ^ Key not recognized; maybe it expired?
  Exception -> ...
  -- ^ Some unknown exception; perhaps the redis store is down?
```

See `examples/CLI.hs` for an example CLI utility.

This needs [Manifest-Redis](https://github.com/avieth/Manifest-Redis) in order
to compile.
