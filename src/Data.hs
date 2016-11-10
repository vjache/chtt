{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Data (
    Msg(..),
    Ping(..),
    Pong(..)
) where

import Control.Distributed.Process
import Data.Binary
import Data.Typeable
import GHC.Generics

data Msg = Msg {msgCreated::Integer, msgValue::Double, msgOrigin::ProcessId} deriving (Show, Eq, Generic, Typeable, Ord)

instance Binary Msg

newtype Ping = Ping ProcessId
  deriving (Typeable, Binary, Show)

newtype Pong = Pong ProcessId
  deriving (Typeable, Binary, Show)
