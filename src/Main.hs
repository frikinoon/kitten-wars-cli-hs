{-# LANGUAGE OverloadedStrings #-}
module Main
       ( main
       ) where

--------------------------------------------------------------------------------
import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Monad            (forever, unless)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Exception        (try)
import           Network.Socket           (withSocketsDo)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Network.WebSockets       as WS

--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app conn = do
  putStrLn "Connected!"

  -- Fork a thread that writes WS data to stdout
  _ <- forkIO $ forever $ do
    result <- try $ WS.receiveData conn :: IO (Either WS.ConnectionException Text)
    case result of
      Left ex -> putStrLn $ "Caught exception when reading: " ++ show ex
      Right val -> liftIO $ T.putStrLn val

  -- Read from stdin and write to WS
  let loop = do
        line <- T.getLine
        result <- try $ unless (T.null line) $ WS.sendTextData conn line :: IO (Either WS.ConnectionException ())
        case result of
          Left ex -> putStrLn $ "Caught exception when sending: " ++ show ex
          Right val -> loop

  loop
  WS.sendClose conn ("Bye!" :: Text)

--------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ WS.runClient "192.168.1.2" 8765 "/" app
