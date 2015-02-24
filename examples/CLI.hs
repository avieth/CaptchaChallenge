{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import CaptchaChallenge
import Manifest.Redis
import System.Environment (getArgs)

myCaptchaChallenge = captchaChallenge defaultConnectInfo

main = do
  args <- getArgs
  case args of 
    ["generate"] -> generate
    ["check", key, solution] -> check (B8.pack key) (B8.pack solution)
    _ -> putStrLn "Command not understood"

generate :: IO ()
generate = do
  (key, image) <- challenge myCaptchaChallenge
  B8.putStrLn key
  BS.writeFile (B8.unpack (BS.append key ".png")) image
  return ()

check :: SecretKey -> Solution -> IO ()
check key proposedSolution = do
  outcome <- solve myCaptchaChallenge (key, proposedSolution)
  print outcome
  return ()
