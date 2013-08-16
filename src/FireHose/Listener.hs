{-# LANGUAGE BangPatterns #-}

module FireHose.Listener
    (
      ClientListener(..)
    ) where

---------------------------------
-- LOCAL
---------------------------------

import FireHose.Util
import FireHose.Model

---------------------------------
-- SITE
---------------------------------

import Network.WebSockets
import Data.Hashable

---------------------------------
-- STDLIB
---------------------------------

data ClientListener p =
    ClientListener
    { listenerClientId :: !Int
    , listenerSink :: Sink p
    }

instance Show (ClientListener p) where
    show (ClientListener cid _) = "ClientListener " ++ (show cid)

instance Eq (ClientListener p) where
    (==) a b = listenerClientId a == listenerClientId b

instance Hashable (ClientListener p) where
    hashWithSalt s c = hashWithSalt s (listenerClientId c)
