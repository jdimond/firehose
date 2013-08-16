{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

---------------------------------
-- LOCAL
---------------------------------

import qualified Data.ListenerTree as LT
import FireHose.Util
import qualified FireHose.Model as M
import FireHose.Commands
import FireHose.Listener

---------------------------------
-- SITE
---------------------------------

import qualified Data.IntMap.Strict as IM
import qualified Data.HashSet as Set
import Network.WebSockets
import qualified Data.Aeson as A
import qualified Network.Socket as S

---------------------------------
-- STDLIB
---------------------------------

import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Lazy as BSL
import Control.Exception (SomeException, handle, Exception)
import Data.Maybe
import qualified Data.List as L

data ServerState p =
    ServerState
    { clientCounter :: !Int
    , listenerTree :: !(LT.ListenerTree T.Text (ClientListener p))
    , clientMap :: IM.IntMap (Set.HashSet (Path, ClientListener p))
    , currentModel :: !M.Model
    }

data Client = Client { clientId :: Int }
    deriving (Show,Eq)

fromRequest :: MVar (ServerState p) -> Request -> IO Client
fromRequest state _ =
    do s <- takeMVar state
       let curId = clientCounter s
       let newclients = IM.insert curId Set.empty $ clientMap s
       putMVar state $ s { clientCounter = curId+1, clientMap = newclients }
       return $ Client curId

newState :: ServerState p
newState = ServerState
           { clientCounter = 0
           , listenerTree = LT.empty
           , clientMap = IM.empty
           , currentModel = A.Null
           }

sendServerMsg :: TextProtocol p => ServerMessage -> WebSockets p ()
sendServerMsg m = sendTextData $ A.encode m

sendSafe :: TextProtocol p => Sink p -> Message p -> IO ()
sendSafe s m = handle ignore $ sendSink s m
    where ignore :: SomeException -> IO ()
          ignore _ = putStrLn "Exception occurred"

sendServerMsgSink :: TextProtocol p => Sink p -> ServerMessage -> IO ()
sendServerMsgSink s m = sendSafe s $ textData $ A.encode m

sendDiffs :: TextProtocol p => [(ClientListener p, M.ModelDiff)] -> IO ()
sendDiffs ls = mapM_ (\(l,d) -> sendSafe (listenerSink l) (diffToDataMsg d)) ls

diffToDataMsg :: TextProtocol p => M.ModelDiff -> Message p
diffToDataMsg d =
    let e p = textData . A.encode . (DataChanged (fromPath p))
    in case d of
        (M.Add p val) -> e p val
        (M.Modify p val) -> e p val
        (M.Delete p) -> e p A.Null

setData :: ServerState p -> Path -> A.Value -> (ServerState p, (Maybe M.ModelDiff, [(ClientListener p, M.ModelDiff)]))
setData s path val = (s { currentModel = nmodel }, (diff, ls))
    where (diff, nmodel) = M.set path val $ currentModel s
          ls = case diff of
                 Nothing -> []
                 Just v -> LT.listenersForDiff v $ listenerTree s

recvLoop :: TextProtocol p => (MVar (ServerState p)) -> Client -> WebSockets p ()
recvLoop state client = forever $ do
    dataMsg <- receiveData
    let clientMsg = A.decode dataMsg
    case clientMsg of
      Just (RegisterListener pathStr) ->
          do let path = toPath pathStr
             sink <- getSink
             curval <- liftIO $ modifyMVar state $ \s ->
                          let listener = ClientListener (clientId client) sink
                              newtree = LT.addListener path listener $ listenerTree s
                              submodel = M.get path $ currentModel s
                              newclients = IM.adjust (Set.insert (path,listener)) (clientId client) $ clientMap s
                          in return (s { listenerTree = newtree, clientMap = newclients}, submodel)
             case curval of
               Just v -> sendServerMsg $ DataChanged (fromPath path) v
               Nothing -> sendServerMsg $ DataChanged (fromPath path) A.Null
      Just (UnregisterListener pathStr) ->
          liftIO $ modifyMVar_ state $ \s ->
              let path = toPath pathStr
                  listener = ClientListener (clientId client) undefined
                  newtree = LT.removeListener path listener $ listenerTree s
                  newclients = IM.adjust (Set.delete (path,listener)) (clientId client) $ clientMap s
              in return (s { listenerTree = newtree, clientMap = newclients })
      Just (SetData pathStr val) -> liftIO $
          do let path = toPath pathStr
             (diff, ls) <- modifyMVar state $ \s -> return $ setData s path val
             sendDiffs ls
      Just (Transaction id pathStr clientold new) ->
          do sink <- getSink
             liftIO $ modifyMVar_ state $ \s ->
               do let path = toPath pathStr
                  let serverold = fromMaybe A.Null $ M.get path (currentModel s)
                  if serverold /= clientold
                     then do sendServerMsgSink sink $ TransactionFailed id
                             return s
                     else do let (newstate, (diff, ls)) = setData s path new
                             sendServerMsgSink sink $ TransactionSuccessful id
                             sendDiffs ls
                             return newstate
      Nothing -> liftIO $ TIO.putStrLn $ T.concat ["Warning! Garbled message: ", T.decodeUtf8 $ BSL.toStrict dataMsg]

app :: MVar (ServerState Hybi00) -> Request -> WebSockets Hybi00 ()
app state req =
    do acceptRequest req
       client <- liftIO $ fromRequest state req
       liftIO $ TIO.putStrLn $ T.concat ["Client connected: ", T.pack $ show (clientId client)]
       flip catchWsError (cleanup client) $ recvLoop state client
    where cleanup client _ = liftIO $
              do putStrLn $ "Client disconnected: " ++ show client
                 modifyMVar_ state $ \s ->
                     let ls = Set.toList $ (clientMap s) IM.! (clientId client)
                         newtree = L.foldl' (\t (p,l) -> LT.removeListener p l t) (listenerTree s) ls
                         newclients = IM.delete (clientId client) $ clientMap s
                     in do putStrLn $ "Removing " ++ (show $ length ls) ++ " listeners"
                           return (s { listenerTree = newtree, clientMap = newclients })


main :: IO ()
main =
    do putStrLn "Running..."
       state <- newMVar newState
       runServer "0.0.0.0" 8000 (app state)
