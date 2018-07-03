{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module JSConverter where
import qualified Control.Monad.Trans.Except as MTE
import qualified Control.Monad.Trans.Maybe  as MTM
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as BS
import           Data.Functor
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import qualified System.Path.NameManip      as SPNM (absolute_path)
data AppCoSimConfigs = AppCoSimConfigs { multiModel  :: String
                                       , coSimConfig :: String}

loadJsonFile :: Prelude.FilePath -> IO (Either String Aeson.Object)
loadJsonFile path = Aeson.eitherDecode <$> BS.readFile path

-- traverse uses the IO Applicative Instance, which does not check for lefts/rights before it is done with all values in the object
-- Switching to exceptT will make it use the exceptT Applicative Instance, which checks for lefts/rights on every value in the object.
-- exceptT thereby does short-circuiting, which is desireable in this case.
crFullPathFmus :: Aeson.Object -> IO (Either String Aeson.Object)
crFullPathFmus obj = case HM.lookup "fmus" obj of
  Nothing -> return . Left $ "key: \"fmus\" does not exist."
  Just (Aeson.Object fmus) -> sequenceA <$> traverse createFullPath fmus
    where
        createFullPath :: Aeson.Value -> IO (Either String Aeson.Value)
        createFullPath (Aeson.String val) =
          (Right . Aeson.String . T.pack) <$> SPNM.absolute_path ("resources/" ++ (T.unpack val))
        createFullPath x = return (Left "Path not of type String")

crMaestroMsgs :: AppCoSimConfigs -> IO (Either String Aeson.Object)
crMaestroMsgs cfgs = MTE.runExceptT $ do
  x <- MTE.ExceptT $ loadJsonFile (multiModel cfgs)
  MTE.ExceptT $ crFullPathFmus x

convertMessages :: AppCoSimConfigs -> IO()
convertMessages cfgs = do
  msgs <- crMaestroMsgs cfgs
  case msgs of
    Left err -> putStrLn $ "Failed with error: " ++ err
    Right x  -> putStrLn $ "Everything Succeeded " ++ (show x)
-- >> BS.writeFile "bla.json" (Aeson.encode x)

-- https://lpaste.net/5815819106753970176
