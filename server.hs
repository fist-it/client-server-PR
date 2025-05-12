{-# LANGUAGE OverloadedStrings #-}

import Network.Socket
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent (forkFinally)
import Data.Char (toUpper)

main :: IO ()
main = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just defaultHints { addrFlags = [AI_PASSIVE] })
                             Nothing (Just "3000")  -- Port 3000
    let serveraddr = head addrinfos

    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 5
    putStrLn "Server listening on port 3000..."

    let loop = do
            (conn, peer) <- accept sock
            putStrLn $ "Connection from " ++ show peer
            forkFinally (talk conn) (\_ -> close conn)
            loop
    loop

talk :: Socket -> IO ()
talk conn = do
    msg <- NSB.recv conn 1024
    if BS.null msg
        then putStrLn "Client disconnected."
        else do
            let upperMsg = BS.map toUpper msg
            NSB.sendAll conn upperMsg
            talk conn

