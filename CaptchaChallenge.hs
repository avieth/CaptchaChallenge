{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module CaptchaChallenge (

    CaptchaChallenge
  , captchaChallenge

  , challenge
  , solve

  , ChallengeOutcome
  , pattern Passed
  , pattern Failed
  , pattern BadKey
  , pattern Exception

  , ChallengePassed

  , SecretKey
  , Solution
  , Challenge

  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base64 as B64
import Control.RichConditional
import Graphics.Captcha
import Crypto.Random
import Manifest.Manifest
import qualified Manifest.Redis as MR
import Data.Proxy

data CaptchaChallenge = CaptchaChallenge {
    redisManifest :: MR.Redis (SecretKey, Solution)
  }

-- | Make a captcha challenge which uses a redis manifest with time-to-live
--   600 seconds, i.e. your captcha challenge must be passed within 600
--   seconds or else you'll have to generate a new one.
captchaChallenge :: MR.ConnectInfo -> CaptchaChallenge
captchaChallenge info = CaptchaChallenge $ MR.redis info (Just 600)

data ChallengePassed = ChallengePassed
  deriving (Show)

data ChallengeOutcome
  = CPassed ChallengePassed
  | CFailed
  | CBadKey
  | CException
  deriving (Show)

pattern Passed x <- CPassed x
pattern Failed <- CFailed
pattern BadKey <- CBadKey
pattern Exception <- CException

type Challenge = BS.ByteString
type SecretKey = BS.ByteString
type Solution = BS.ByteString

instance Manifestible (SecretKey, Solution) where
  type ManifestibleKey (SecretKey, Solution) = SecretKey
  type ManifestibleValue (SecretKey, Solution) = Solution
  manifestibleKey = fst
  manifestibleValue = snd
  manifestibleFactorization = (,)

-- | A secret key is 8 random bytes, base64 encoded.
--   The choice of 8 was arbitrary.
makeSecretKey :: IO SecretKey
makeSecretKey = do
  ep <- createEntropyPool
  let gen = cprgCreate ep :: SystemRNG
  let (rand, _) = cprgGenerate 8 gen
  return $ B64.encode rand

challenge :: CaptchaChallenge -> IO (SecretKey, Challenge)
challenge c = do
    secret <- makeSecretKey
    (solutionString, captcha) <- makeCaptcha
    let solution = B8.pack solutionString
    manifest (redisManifest c) (mput (secret, solution) (Proxy :: Proxy MR.Redis))
    return $ (secret, captcha)

solve :: CaptchaChallenge -> (SecretKey, Solution) -> IO (ChallengeOutcome)
solve c (secret, proposedSolution) = do
    (fetch, _) <- manifest (redisManifest c) (mget secret (Proxy :: Proxy (MR.Redis (SecretKey, Solution))))
    case fetch of
      Left _ -> return CException
      Right x -> inCase x (found c proposedSolution) notFound

  where

    found :: CaptchaChallenge -> Solution -> (SecretKey, Solution) -> IO (ChallengeOutcome)
    found c s (secretKey, s') =
      if s == s'
      then do
        manifest (redisManifest c) (mdel secretKey (Proxy :: Proxy (MR.Redis (SecretKey, Solution))))
        return $ CPassed ChallengePassed
      else return CFailed

    notFound :: IO (ChallengeOutcome)
    notFound = return CBadKey
