module PeerMonitor (
    PeerMonitor.init
) where

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad
import Control.Applicative
import Control.Concurrent (threadDelay)
import System.Random
import System.Locale
import Data.Time
import Data.Time.Clock.POSIX
import Data.List


import Data


init :: ProcessId -> Backend -> Process ()
init pid be = loop pid be []

loop :: ProcessId -> Backend -> [NodeId] -> Process ()
loop pid be peers = do
    nid <- getSelfNode
    peers' <- (sort . nub . (nid:)) <$> findPeersL be
    if peers == peers'
    then do loop pid be peers
    else do
        say $ "Peers changed: " ++ (show peers')
        if peers' == [nid]
             then loop pid be peers'
             else do
                let nextNid = seekNextPeer nid peers'
                say $ "Next peer updated: " ++ (show nextNid)
                send pid nextNid
                loop pid be peers'

findPeersL :: Backend -> Process [NodeId]
findPeersL be = liftIO $ findPeers be 1000000

seekNextPeer :: NodeId -> [NodeId] -> NodeId
seekNextPeer nid peers@(_:_:_) = seekNextPeer_ nid peers $ head peers

seekNextPeer_ :: Eq e => e -> [e] -> e -> e
seekNextPeer_ nid (p:next:peers) first
    | p == nid  = next
    | otherwise = seekNextPeer_ nid (next:peers) first
seekNextPeer_ nid [nid'] first | nid == nid' = first
