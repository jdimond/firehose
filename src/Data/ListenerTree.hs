{-# LANGUAGE BangPatterns #-}

module Data.ListenerTree
    (
      ListenerTree, empty
    , addListener, removeListener, listenersForDiff
    ) where

---------------------------------
-- LOCAL
---------------------------------

import FireHose.Model

---------------------------------
-- SITE
---------------------------------

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable

---------------------------------
-- STDLIB
---------------------------------

import Prelude hiding (null)
import qualified Data.Text as T
import qualified Data.List as L

instance (Show p, Hashable p, Eq p, Show l) => Show (ListenerTree p l) where
    show t = show' 0 t
        where show' i (Tree m l) =
                  let ls = indent i $ show l
                      child x = (indent i $ show x) ++ "\n" ++ (show' (i+2) $ m HM.! x)
                  in L.intercalate "\n" $ ls:(map child $ HM.keys m)
              indent x str = (take x $ repeat ' ') ++ str


data ListenerTree p l = Tree !(HM.HashMap p (ListenerTree p l)) !(HS.HashSet l)

empty :: ListenerTree p l
empty = Tree HM.empty HS.empty

null :: ListenerTree p l -> Bool
null (Tree m ls) = HM.null m && HS.null ls

attach :: (Eq p, Hashable p) => p -> ListenerTree p l -> ListenerTree p l -> ListenerTree p l
attach p t (Tree m ls) = Tree nm ls
    where nm = HM.insert p t m

detach :: (Eq p, Hashable p) => p -> ListenerTree p l -> ListenerTree p l
detach p (Tree m ls) = Tree nm ls
    where nm = HM.delete p m

listenersForDiff :: ModelDiff -> ListenerTree T.Text l -> [(l,ModelDiff)]
listenersForDiff md t = go (pathForDiff md) md t
    where go [] d (Tree m ls) = report d ls ++ (concatMap (\(e,st) -> go [] (stepPath e d) st) $ HM.toList m)
          go (p:ps) d (Tree m ls) = report d ls ++ (go ps d $ HM.lookupDefault empty p m)
          report d = map (flip (,) d) . HS.toList

addListener :: (Eq p, Hashable p, Eq l, Hashable l) => [p] -> l -> ListenerTree p l -> ListenerTree p l
addListener path !l tree = down path tree []
    where down [] (Tree m ls) ns = up ns (Tree m (HS.insert l ls))
          down (p:ps) t@(Tree m _) ns =
            let !subtree = HM.lookupDefault empty p m
            in down ps subtree ((p,t):ns)
          up [] t = t
          up ((p,t):ns) st = up ns (attach p st t)

removeListener :: (Eq p, Hashable p, Eq l, Hashable l) => [p] -> l -> ListenerTree p l -> ListenerTree p l
removeListener path !l tree = down path tree []
    where down [] (Tree m ls) ns = up ns (Tree m (HS.delete l ls))
          down (p:ps) t@(Tree m _) ns =
            case HM.lookup p m of
              Nothing -> tree
              Just st -> down ps st ((p,t):ns)
          up [] t = t
          up ((p,t):ns) st =
              if null st
                 then up ns (detach p t)
                 else up ns (attach p st t)
