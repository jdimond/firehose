{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module FireHose.Model
    (
      Model, ModelDiff(..)
    , set, get, diff, stepPath, pathForDiff
    ) where

---------------------------------
-- LOCAL
---------------------------------

import FireHose.Util

---------------------------------
-- SITE
---------------------------------

import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

---------------------------------
-- STDLIB
---------------------------------

import qualified Data.Text as T

type Model = A.Value
type ObjectMap = HM.HashMap T.Text Model

set :: Path -> Model -> Model -> (Maybe ModelDiff, Model)
set path v oldv = go path oldv
  where go [] curobj = (diff (reverse path) curobj v, v)
        go (p:ps) curobj =
            let curmap =
                    case curobj of
                      (A.Object m) -> m
                      _ -> HM.empty
                (curdiff, subtree) = go ps (HM.lookupDefault A.emptyObject p curmap)
            in (curdiff, A.Object (HM.insert p subtree curmap))

get :: Path -> Model -> Maybe Model
get [] m = Just m
get (p:ps) (A.Object m) = HM.lookup p m >>= get ps
get _ _ = Nothing

data ModelDiff =
    Delete !Path
  | Add !Path !Model
  | Modify !Path !Model
     deriving (Eq, Show)

pathForDiff :: ModelDiff -> Path
pathForDiff (Delete p) = p
pathForDiff (Add p _) = p
pathForDiff (Modify p _) = p

stepPath :: T.Text -> ModelDiff -> ModelDiff
stepPath e (Delete p) = (Delete $ p++[e])
stepPath e (Add p (A.Object m)) =
    case HM.lookup e m of
      Just v -> (Add (p++[e]) v)
      Nothing -> (Delete $ p++[e])
stepPath e (Modify p (A.Object m)) =
    case HM.lookup e m of
      Just v -> (Modify (p++[e]) v)
      Nothing -> (Delete $ p++[e])
stepPath e (Add p _) = (Delete $ p++[e])
stepPath e (Modify p _) = (Delete $ p++[e])

diff :: Path -> Model -> Model -> Maybe ModelDiff
diff p (A.Object oldo) (A.Object newo) =
    let deleted = HM.difference oldo newo
    in case HM.keys deleted of
         [] -> diffObject p oldo newo Nothing
         [e] -> diffObject p oldo newo (Just $ Delete $ reverse (e:p))
         _ -> Just $ Modify (reverse p) (A.Object newo)
diff p oldv newv =
    if oldv == newv
       then Nothing
       else Just $ Modify (reverse p) newv

--TODO: BFS
diffObject :: Path -> ObjectMap -> ObjectMap -> Maybe ModelDiff -> Maybe ModelDiff
diffObject path oldo newo curdiff = go (HM.toList newo) curdiff
    where go [] cdiff = cdiff
          go ((nk,nv):nvs) cdiff =
              case HM.lookup nk oldo of
                Just ov ->
                  let d = diff (nk:path) ov nv
                  in case (d,cdiff) of
                       (Nothing,od) -> go nvs od
                       (od,Nothing) -> go nvs od
                       _ -> Just $ Modify (reverse path) (A.Object newo)
                Nothing ->
                  case cdiff of
                    Nothing -> go nvs (Just $ Add (reverse (nk:path)) nv)
                    _ -> Just $ Modify (reverse path) (A.Object newo)
