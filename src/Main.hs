{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.Environment (getArgs)

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
-- import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process.Backend.SimpleLocalnet --(Backend, initializeBackend, newLocalNode)

import Data
import MessageGenerator
import MessageRetranslator
import PeerMonitor


data Settings = Settings {host :: String, port :: String, sendFor :: Int, waitFor :: Int, withSeed :: Int}

init :: Backend -> Int -> Process (ProcessId, ProcessId)
init be seed = do
    pid <- getSelfPid
    rtPid  <- spawnLocal $ MessageRetranslator.init
    _      <- spawnLocal $ PeerMonitor.init rtPid be
    genPid <- spawnLocal $ MessageGenerator.init seed rtPid
    return (rtPid, genPid)

main :: IO ()
main = do
    args <- getArgs
    let getArg   = getArg_ args
        host     = getArg "--host" "127.0.01"
        port     = getArg "--port" "1234"
        sendFor  = getArg "--send-for" "30"
        waitFor  = getArg "--waitFor"  "5"
        withSeed = getArg "--with-seed" "0"

    let rt = initRemoteTable
    be <- initializeBackend host port rt
    node <- newLocalNode be
    runProcess node $ do
        selfPid <- getSelfPid
        (rtPid, genPid) <- Main.init be $ read withSeed
        liftIO $ threadDelay ((read sendFor)*1000000)
        exit genPid "finish"
        liftIO $ threadDelay ((read waitFor)*1000000)
        send rtPid (selfPid, "finish")
        m <- expectTimeout  1000000 :: Process (Maybe (Int, Double))
        case m of
            Nothing    -> liftIO $ putStrLn $ "SCORE RCV TIMEOUT: EXIT!"
            Just score -> liftIO $ putStrLn $ "SCORE: " ++ (show score)
        liftIO $ threadDelay (1*1000000)

getArg_ :: [String] -> String -> String -> String
getArg_ (k:v:other) k' def
    | k == k' = v
    | otherwise = getArg_ other k' def
getArg_ [] k' def = def
