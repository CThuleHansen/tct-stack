{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ScenarioRunner where
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy      as B
import qualified Data.HashMap.Strict       as Hm
import           Data.Text                 as T
import           Filesystem.Path
import           GHC.Generics
import           Network.HTTP.Simple
import           System.Path.NameManip     as FS (absolute_path)

-- Check out maybeT
runScenario :: IO ()
runScenario = do
  id <- createSession
  case id of
    Nothing -> putStrLn "Failed to retrieve sessionID."
    Just x  ->
      putStrLn ("Session ID: " ++ x) >>
      putStrLn "Initialising" >>
      initialise x >>
      simulate x >>
      putStrLn "Finished"

data CreateSession = CreateSession {sessionId :: String} deriving (Show, Generic)
instance FromJSON CreateSession

-- Create session for the co-simulation
createSession :: IO (Maybe String)
createSession =  do
  (request :: Request) <- parseRequest "http://localhost:8082/createSession"
  (response :: Response (Either JSONException CreateSession)) <- httpJSONEither request
  case getResponseBody response of
    Left ex -> (putStrLn $ "CreateSession Failed: " ++ show ex) >> return Nothing
    Right crSession -> (putStrLn $ "Session ID: " ++ sessionId crSession) >> return (Just $ sessionId crSession)

-- Initialise the co-simulation by sending configuration file
-- Object is type synonym for HashMap Text Value
initialise :: String -> IO (Maybe B.ByteString)
initialise sessionId = runMaybeT $
    MaybeT (loadJSONFile "resources/initialise_body.json") >>=
    ( MaybeT . calcInitJson'  >=>
    MaybeT .  (postJSON ("http://localhost:8082/initialize/" ++ sessionId)) )

simulate :: String -> IO (Maybe B.ByteString)
simulate sessionId = runMaybeT $
  MaybeT (loadJSONFile "resources/simulate_body.json") >>=
  (MaybeT . (postJSON ("http://localhost:8082/simulate/" ++ sessionId)))

loadJSONFile :: String -> IO (Maybe Object)
loadJSONFile fName = loadJson fName
  >>= (\case
          Left error -> putStrLn ("Failed to load json file: " ++ "fName" ++ ". Error: " ++ error)
            >> return Nothing
          Right (Object json) -> return $ Just json)
  where
    loadJson :: Prelude.FilePath -> IO (Either String Value)
    loadJson path = B.readFile path >>= return . eitherDecode

postJSON :: String -> Object -> IO (Maybe B.ByteString)
postJSON url body = do
  request :: Request <- parseRequest url
  response <- let request' = setRequestMethod "POST" $ setRequestBodyJSON body $ request in
    httpLBS request'
  let responseStatus = getResponseStatusCode response in
    case responseStatus == 200 of
    False -> putStr ("postJSON to url:" ++ url ++ " failed with response: 200\r\n") >> return Nothing
    True ->  putStr ("Response status code: " ++ show (getResponseStatusCode response) ++ "\r\n") >>
      let response' = getResponseBody response in
        putStr ("ResponseBody: " ++ (show response) ++ "\r\n") >> return (Just response')

calcInitJson' :: Object -> IO (Maybe Object)
calcInitJson' json = let (eitherFmusJson :: Either String (IO Object)) = updateJsonFmus json in
  case eitherFmusJson of
    Left err -> putStrLn err >> return Nothing
    Right (ioFmusJson :: IO Object)  ->
      ioFmusJson >>=
      (\(fmusJson) ->
         let (json' :: Object) = Hm.insert "fmus" (Object fmusJson) json in
           (putStrLn . show) json' >> (return . Just) json')

-- Traverses the fmus object and creates full paths for the values.
updateJsonFmus :: Object -> Either String (IO Object)
updateJsonFmus (json :: Object) =
  case Hm.lookup "fmus" json of
    Nothing -> Left "key: \"fmus\" does not exist."
    Just (Object fmus) -> Right (traverse createFullPath fmus)
      where
        createFullPath :: Value -> IO Value
        createFullPath (String val) =
          (String . T.pack) <$> FS.absolute_path ("resources/" ++ (T.unpack val))
        createFullPath x = pure x -- Ignore non strings
