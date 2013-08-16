{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FireHose.Commands
    (
      ClientMessage(..), ServerMessage(..)
    ) where

---------------------------------
-- LOCAL
---------------------------------

import FireHose.Util

---------------------------------
-- SITE
---------------------------------

import qualified Data.Aeson as A
import Data.Aeson.TH
import qualified Data.Aeson.Types as A

---------------------------------
-- STDLIB
---------------------------------

import qualified Data.Text as T

data ClientMessage =
      RegisterListener T.Text
    | UnregisterListener T.Text
    | SetData T.Text A.Value
    | Transaction Int T.Text A.Value A.Value
    deriving (Show, Eq)

data ServerMessage =
      DataChanged T.Text A.Value
    | TransactionSuccessful Int
    | TransactionFailed Int
    deriving (Show, Eq)

$(deriveJSON id ''ClientMessage)
$(deriveJSON id ''ServerMessage)
