{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO

main :: IO ()
main = runTcpClient "127.0.0.1" "3000" $ \s -> do
  putStrLn "Enter messages to send to server (empty message to quit):"
  loop s
  where
    loop sock = do
      putStr "> "
      hFlush stdout
      input <- getLine
      if null input
        then putStrLn "Disconnecting..."
        else do
          sendAll sock (C.pack input)
          response <- recv sock 1024
          putStr "Server: "
          C.putStrLn response
          loop sock

runTcpClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTcpClient host port client = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close client
  where
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      NE.head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
      sock <- openSocket addr
      connect sock $ addrAddress addr
      return sock
