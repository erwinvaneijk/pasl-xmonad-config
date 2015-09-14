{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TupleSections #-}
-- | Workaround to inability to use ForkIO in xmonad
-- Basicly it forks execute computation serializes and writes into shared
-- file handler wich is located on tmpfs filesystem, and sends notification
-- to root window when computation is finished. Obviosly you cannot use
-- IORef and such in this asynchronous computation.
module XMonad.Powerbar.Async
    ( Async
    , async
    , asyncRun
    , asyncHook
    ) where

import XMonad.Powerbar.Lens
import Control.Exception      (SomeException, tryJust, bracket)
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson             (ToJSON, FromJSON, encode, decode)
import Data.Monoid            (All(..))
import Data.Typeable          (Typeable)
import System.Directory       (removeFile)
import System.IO              (IOMode(..), withBinaryFile)
import Text.Printf            (printf)
import qualified Data.ByteString.Lazy     as B
import qualified Data.Map                 as M
import qualified XMonad                   as X
import qualified Control.Monad.Trans.Cont as C
import XMonad.Powerbar.Config (extState)


-- | Asynchronous continuation
type Async a = C.ContT () X.X a

data AsyncQueue =
    AsyncQueue { _asyncQueue :: M.Map Int (X.X ())
               , _asyncUID   :: Int
               } deriving Typeable

asyncQueue :: Lens' AsyncQueue (M.Map Int (X.X ()))
asyncQueue f v = (\c -> v {_asyncQueue = c}) `fmap` f (_asyncQueue v)

asyncUID :: Lens' AsyncQueue Int
asyncUID f v = (\c -> v {_asyncUID = c}) `fmap` f (_asyncUID v)

instance X.ExtensionClass AsyncQueue where
    initialValue = AsyncQueue M.empty 0

-- | Run asynchronous computation
asyncRun :: Async a -> X.X ()
asyncRun cont = C.runContT cont (const . return $ ())


-- | Execute IO asynchronously
async :: (ToJSON a, FromJSON a) => IO a -> Async a
async comp = C.ContT $ \done ->
             do uid <- extState . asyncUID %%= \uid -> (uid, uid + 1)
                let resultFile = printf "/dev/shm/xmonad-async-%d" uid
                    asyncAction :: X.X ()
                    asyncAction = do
                      text <- liftIO (B.readFile resultFile)
                      liftIO (removeFile resultFile)
                      case decode text of
                        Nothing          -> logError "failed to parse result"
                        Just (Left err)  -> logError err
                        Just (Right val) -> done val
                extState . asyncQueue %= M.insert uid asyncAction
                -- save result
                X.xfork $ do
                  result <- tryJust (\e -> Just $ show (e :: SomeException)) comp
                  withBinaryFile resultFile WriteMode $ flip B.hPut (encode result)
                  asyncNotify uid
                return ()


logError :: String -> X.X ()
logError = X.trace . ("[xmonad.async] error: " ++)


-- | Notify xmonad that computation with specified uid has completed
asyncNotify :: (MonadIO m) => Int -> m ()
asyncNotify uid =
    liftIO $ bracket (X.openDisplay "") X.closeDisplay $ \dpy -> do
      root <- X.rootWindow dpy (X.defaultScreen dpy)
      atom <- X.internAtom dpy "XMONAD_ASYNC" False
      X.allocaXEvent $ \ev -> do
        X.setEventType ev X.clientMessage
        X.setClientMessageEvent ev root atom 32 (fromIntegral uid) X.currentTime
        X.sendEvent dpy root False X.structureNotifyMask ev


-- | Hook wich is used to notify about completion of some asynchronous computation
asyncHook :: X.Event -> X.X All
asyncHook (X.ClientMessageEvent {X.ev_message_type = mt , X.ev_data = dt})
    = do atom <- X.getAtom "XMONAD_ASYNC"
         when (mt == atom && dt /= []) $
              do let uid = fromIntegral . head $ dt
                 actionMaybe <- extState . asyncQueue . at uid %%= (,Nothing)
                 maybe (logError $ "unhandled action: " ++ show uid)
                       (X.userCodeDef ()) actionMaybe
         return $ All True
asyncHook _ = return $ All True
