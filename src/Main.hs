{-# LANGUAGE OverloadedStrings #-}
module Main
       ( main
       ) where

--------------------------------------------------------------------------------
import           Control.Concurrent       (forkIO, killThread, myThreadId)
import           Control.Monad            (forever)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Exception        (try)
import           Network.Socket           (withSocketsDo)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Network.WebSockets       as WS
import           Data.Aeson
import           Data.ByteString.Lazy.Internal (ByteString)

import Protocol

--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app conn = do
  putStrLn "Connected!"

  -- Fork a thread that writes WS data to stdout
  _ <- forkIO $ forever $ do
    result <- try $ WS.receiveData conn :: IO (Either WS.ConnectionException Text)
    case result of
      Left _ -> killThread =<< myThreadId
      Right val -> liftIO $ T.putStrLn val

  -- Read from stdin and write to WS
  let loop = do
        putStrLn "Type in your action: "
        line <- T.getLine
        action <- handleInput $ T.unpack line
        case action of
          Nothing -> putStrLn "Invalid input. Try again:"
          Just a -> WS.sendTextData conn a :: IO ()
        loop

  _ <- loop
  WS.sendClose conn ("Bye!" :: Text)

handleInput :: String -> IO (Maybe ByteString)
handleInput "move" = pure $ Just $ encode $ mkMove "Kev" MvUp
handleInput _ = pure Nothing

--------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ WS.runClient "192.168.1.2" 8080 "/" app
