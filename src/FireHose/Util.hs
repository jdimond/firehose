{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module FireHose.Util
    (
      Path
    , fromPath, toPath
    ) where

---------------------------------
-- LOCAL
---------------------------------

---------------------------------
-- SITE
---------------------------------

---------------------------------
-- STDLIB
---------------------------------

import qualified Data.Text as T

type Path = [T.Text]

fromPath :: [T.Text] -> T.Text
fromPath = T.intercalate "/"

toPath :: T.Text -> [T.Text]
toPath = filter (not . T.null) . T.split (=='/')
