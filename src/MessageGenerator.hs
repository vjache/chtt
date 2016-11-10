module MessageGenerator (
    MessageGenerator.init
) where

import Control.Distributed.Process
import Control.Monad
import Control.Applicative
import Control.Concurrent (threadDelay)
import System.Random
import System.Locale
import Data.Time
import Data.Time.Clock.POSIX

import Data


init :: Int -> ProcessId -> Process ()
init s pid =
    let g  = mkStdGen s
        ms = randomRs (0::Double,1::Double) g
    in forM_ ms $ \m -> do
        t <- liftIO $ (round . (*1000000)) <$> getPOSIXTime
        self <- getSelfPid
        send pid $ (self, Msg {msgValue = m, msgCreated = t, msgOrigin = pid})
        liftIO $ threadDelay (1000000) -- 1 message per sec