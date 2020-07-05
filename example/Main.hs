{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import           Control.Concurrent
                 (threadDelay)
import           Control.Monad.IO.Class
                 (MonadIO (..))
import           Control.Monad.Trans.Resource
                 (ResourceT)
import qualified Data.ByteString              as BS
import           Data.Maybe
                 (fromMaybe)
import           Network.HTTP.Client
                 (defaultManagerSettings, newManager)
import           Network.Wai
                 (Application)
import           System.Environment
                 (getArgs, lookupEnv)
import           Text.Read
                 (readMaybe)

import qualified Streamly
import qualified Streamly.External.ByteString as Streamly
import qualified Streamly.FileSystem.Handle as Streamly
import qualified Streamly.Prelude as Streamly
import           Servant
import           Servant.Client.Streaming
import           Servant.Streamly
import           System.IO

import qualified Network.Wai.Handler.Warp     as Warp

type FastAPI t = "get" :> Capture "num" Int :> StreamGet NewlineFraming JSON (t IO Int)

type API t = FastAPI t
    :<|> "slow" :> Capture "num" Int :> StreamGet NewlineFraming JSON (t IO Int)
    -- monad can be ResourceT IO too.
    :<|> "readme" :> StreamGet NoFraming OctetStream (t (ResourceT IO) BS.ByteString)
    -- we can have streaming request body
    :<|> "proxy"
        :> StreamBody NoFraming OctetStream (t IO BS.ByteString)
        :> StreamPost NoFraming OctetStream (t IO BS.ByteString)

api :: Proxy (API Streamly.SerialT)
api = Proxy

server :: Server (API Streamly.SerialT)
server = fast :<|> slow :<|> readme :<|> proxy
  where
    fast n = liftIO $ do
        putStrLn $ "/get/" ++ show n
        return $ fastS n

    slow n = liftIO $ do
        putStrLn $ "/slow/" ++ show n
        return $ slowS n

    readme = liftIO $ do
        putStrLn "/proxy"
        withFile "README.md" ReadMode $
          return . Streamly.map Streamly.fromArray . Streamly.unfold Streamly.readChunks

    proxy c = liftIO $ do
        putStrLn "/proxy"
        return c

    fastS = Streamly.unfoldr mk where
        mk m
            | m < 0     = Nothing
            | otherwise = Just (m, pred m)

    slowS = Streamly.mapM (\x -> threadDelay 1000000 >> return x) . fastS

app :: Application
app = serve api server

cli :: Client ClientM (FastAPI Streamly.SerialT)
cli :<|> _ :<|> _ :<|> _ = client api

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("server":_) -> do
            putStrLn "Starting servant-streamly:example at http://localhost:8000"
            port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            Warp.run port app
        ("client":ns:_) -> do
            n <- maybe (fail $ "not a number: " ++ ns) pure $ readMaybe ns
            mgr <- newManager defaultManagerSettings
            burl <- parseBaseUrl "http://localhost:8000/"
            withClientM (cli n) (mkClientEnv mgr burl) $ \case
                Left err -> print err
                Right c  -> do
                    x <- Streamly.foldl' (\p _ -> p + 1) (0 :: Int) c
                    print x
        _ -> do
            putStrLn "Try:"
            putStrLn "stack exec servant-streamly-example -- server"
            putStrLn "stack exec servant-streamly-example -- client 10"
            putStrLn "time curl -H 'Accept: application/json' localhost:8000/slow/5"
