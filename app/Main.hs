{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import qualified Control.Exception          as E
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Functor
import           Data.String
import qualified JSConverter                as JSC
import           Network.HTTP.Simple
import qualified ScenarioRunner             as SR
import           System.Directory
import           System.IO
import           System.IO.Error
import           System.Process

--main :: IO ()
--main = do
--  initialise
--  hdl <- launchMaestro
--  case hdl of
--    Just h -> SR.runScenario >> putStrLn "Terminating Maestro" >> terminateProcess h >> threadDelay 1000000 >> showIsOfflineIO isOffline
--    _ -> return ()
main :: IO ()
main = let cfgs = JSC.AppCoSimConfigs {JSC.multiModel = "resources/initialise_body.json", JSC.coSimConfig="resources/simulate_body.json"} in
  JSC.convertMessages cfgs

-- Delete the log file
initialise = do
  putStrLn "Deleting log file"
  deleteFileResult <- (fmap Just $ removeFile "MaestroProcessOutput") `E.catch` handleEx
  case deleteFileResult of
    Nothing -> putStrLn "Log file did not exist"
    Just  _ -> pure ()
  where handleEx e
          | isDoesNotExistError e = return Nothing
          | otherwise = E.throwIO e

-- Arg 2 is a function and invoked with arg 1, afterwards original arg 1 is returned.
redundant :: IO (a) -> (a -> IO ()) -> IO (a)
redundant x f = x >>= (\v -> f v $> v)

-- Launches Maestro and pipes the output into a log file
launchMaestro :: IO (Maybe ProcessHandle)
launchMaestro = do
  offline <- redundant isOffline showIsOffline
  if offline
    then
        do
          putStrLn "Creating log file"
          fHdl <- openFile "MaestroProcessOutput" WriteMode
          putStrLn "Launching Maestro"
          (_, _, _, hdl) <- createProcess (proc "Java" ["-jar", "resources/maestro.jar"]) {std_out = UseHandle fHdl}
          threadDelay 2000000 -- 2 seconds
          showIsOfflineIO isOffline
          return $ Just hdl
    else
        do
          return Nothing

showIsOfflineIO :: IO (Bool) -> IO ()
showIsOfflineIO x = do
  offline <- x
  showIsOffline offline

-- Prints an offline message
showIsOffline :: Bool -> IO ()
showIsOffline offline = putStrLn . ("Maestro is " ++) $ if offline then "offline" else "online"

-- Determines whether Maestro is offline
-- fromString can be used instead of {-# LANGUAGE OverloadedStrings #-}
isOffline :: IO (Bool)
isOffline = do
  response <- (fmap Just $ httpLBS (fromString "http://localhost:8082")) `E.catch`
    (\(HttpExceptionRequest req content) -> return Nothing)
  case response of
    Nothing -> return True
    Just _  -> return False
