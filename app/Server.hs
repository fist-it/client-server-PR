{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toUpper)
import qualified Data.List.NonEmpty as NE
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main =
  runTcpServer Nothing "3000" talk
  where
    talk conn = do
      msg <- recv conn 1024
      unless (BS.null msg) $ do
        sendAll conn $ BS.map toUpper msg
        talk conn

runTcpServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTcpServer mhost port talk = withSocketsDo $ do
  serveraddr <- resolve
  E.bracket (open serveraddr) close loop
  where
    resolve = do
      let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
      NE.head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 5
      putStrLn $ "Server listening on " ++ port ++ "..."
      return sock
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
        \(conn, peer) -> void $ do
          putStrLn $ "Connection from " ++ show peer
          forkFinally (talk conn) $ \result -> do
            case result of
              Left err -> putStrLn $ "Client connection error: " ++ show err
              Right _ -> putStrLn $ "Client disconnected from " ++ show peer
            gracefulClose conn 5000
