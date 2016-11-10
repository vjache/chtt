module MessageRetranslator (
    MessageRetranslator.init
) where

import Control.Distributed.Process
import Control.Monad
import Control.Applicative
import System.Random
import System.Locale
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Set as S

import Data

data State = State {nextPeers :: [NodeId], msgCache :: S.Set Msg}

regName = "retran"

init :: Process ()
init = do
    selfPid <- getSelfPid
    register regName selfPid
    loop $ State{nextPeers = [], msgCache = S.empty}

loop :: State -> Process ()
loop state = do
    state' <- receiveWait [match $ handleNewMsg state,
                           match $ handleRetranslate state,
                           match $ handleNextPeerChanged state,
                           match $ handleFinish state]
    loop state'

handleFinish :: State -> (ProcessId, String) -> Process State
handleFinish state@State{msgCache = cache} (pid, "finish") = do
    let msgs = S.toList cache
        len  = length msgs
        is   = iterate (+1.0) 1.0 :: [Double]
        s    = sum [ i*m | (i, Msg{msgValue = m}) <- zip is msgs] :: Double
    send pid (len, s)
    return state

{-
    Store newly generated message in a cache and send to the retranslator of the next node.
 -}
handleNewMsg :: State -> (ProcessId, Msg) -> Process State
handleNewMsg state (_, newMsg) = do
    passMsgToNextPeer state newMsg

cacheMsg :: State -> Msg -> State
cacheMsg state msg = state {msgCache = S.insert msg $ msgCache state}

handleRetranslate :: State -> Msg -> Process State
handleRetranslate state msg = do
    if S.notMember msg $ msgCache state
    then do say $ "RCV(retran): " ++ (show msg)
            passMsgToNextPeer state msg
    else do say $ "RCV(no retran): " ++ (show msg)
            return state

passMsgToNextPeer :: State -> Msg -> Process State
passMsgToNextPeer state msg = do
    let state' = cacheMsg state msg
    forM_ (nextPeers state') $ \nid -> nsendRemote nid regName msg
    return state'

handleNextPeerChanged :: State -> NodeId -> Process State
handleNextPeerChanged state nid = do
    if (nextPeers state) == [nid]
    then return state
    else do
        let state' = state{nextPeers = [nid]}
        forM_ (S.toList $ msgCache state') $ passMsgToNextPeer state'
        return state'