{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where
import qualified Data.Text          as T
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import Control.Monad
import Control.Concurrent (forkIO)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Search as Search

parseWant :: LBS.ByteString -> Maybe T.Text
parseWant (Search.indices "hr" -> (x:_)) = Just "I think you'll find that's be retro-branded to personnel"
parseWant phrase = Just $ decodeUtf8 $ LBS.toStrict $ LBS.reverse phrase

application :: WS.ServerApp
application pending = do
  conn <- WS.acceptRequest pending
  let loop wants = do
              msg <- WS.receiveDataMessage conn
              case msg of
                WS.Text "BACK" -> do
                  mapM_ (WS.sendTextData conn) wants
                  loop wants
                WS.Text (parseWant -> Just want) -> do
                  WS.sendTextData conn (want :: T.Text)
                  loop (want : wants)
                _ -> loop wants
  loop []

main :: IO ()
main = do
    putStrLn "http://localhost:8080"
    Warp.runSettings
      (Warp.setPort 8080 Warp.defaultSettings)
      $ WaiWS.websocketsOr WS.defaultConnectionOptions application undefined
