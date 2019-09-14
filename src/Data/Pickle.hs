{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Pickle ( Tags(..)
                   , StatsDConfig(..)
                   , MetricData
                   , defaultConfig
                   , setupPickle
                   , metric
                   , gauge
                   , counter
                   , timer
                   , showT
                   )
where

import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Control.Exception
import Control.Monad
import Network.Socket hiding (send)
import Network.Socket.ByteString (send)
import Control.Monad.IO.Class
import System.IO.Unsafe
import Control.Concurrent.STM

-- | Tags for DogStatsD
type Tags = M.Map T.Text T.Text

-- | Configuration for the UDP connection used
data StatsDConfig = StatsDConfig { statsdHost      :: T.Text -- ^ Host of statsd server
                                 , statsdPort      :: T.Text -- ^ Port of statsd server
                                 , statsdPrefix    :: T.Text -- ^ Prefix concatenated to all metrics names in our program
                                 , statsdTags      :: Tags   -- ^ 'mappend'-ed tags for all stats we report
                                 , statsdVerbose   :: Bool   -- ^ Whether to print all metrics to stdout
                                 }

-- | 'Pickle' is our Data Dog (get it?) and he holds on to our sock and config.
--   `pickle` is a little MVar bed which Pickle likes to sleep in. He is a good boy.
data Pickle = Pickle { pickleSock :: Socket
                     , pickleCfg  :: StatsDConfig
                     }

-- | Something that can be sent as a metric.
type MetricData a = (Show a, Real a)

-- | Default config used for StatsD UDP connection ()
defaultConfig :: StatsDConfig
defaultConfig = StatsDConfig { statsdHost    = "127.0.0.1"
                             , statsdPort    = "8125"
                             , statsdPrefix  = ""
                             , statsdTags    = M.empty
                             , statsdVerbose = False
                             }

{-|
Start up our statsd client. You probably want to do this first in main:

 > main = do
       setupPickle defaultConfig
       ...

Subsequent calls to 'setupPickle' will close the existing connection and create
a new one with the updated settings. If multiple threads race to setup the
connection, the last one to finish wins.
-}
setupPickle :: StatsDConfig -> IO ()
setupPickle cfg = bracketOnError checkPickle (const finish) setPickle
    where checkPickle = atomically $ do
            gp <- readTVar pickle
            -- Someone else is setting up a connection, wait for them:
            when (gpSetupRunning gp) retry
            writeTVar pickle (gp { gpSetupRunning = True})
            pure (gpPickle gp)
          -- If anything bad happens, unblock other 'setupPickle' callers.
          finish = atomically $ do
            modifyTVar' pickle (\gp -> gp { gpSetupRunning = False })
          setPickle Nothing = do
            pick <- initPickle cfg
            atomically $ do
                writeTVar pickle (GlobalPickle False (Just pick))
          setPickle (Just oldPick) = do
              newPick <- initPickle cfg
              atomically $ do
                  writeTVar pickle (GlobalPickle False (Just newPick))
              -- Close the old pickle connection once we're done.
              close (pickleSock oldPick)

-- | Send a gauge.
gauge :: (MetricData a) => T.Text -> a -> Maybe Tags -> IO()
gauge name val mTags = metric "g" name val mTags Nothing
-- | alias for gauge since it can be hard to spell.
gage :: (MetricData a) => T.Text -> a -> Maybe Tags -> IO()
gage  = gauge
-- | alias for gauge since it can be hard to spell.
guage :: (MetricData a) => T.Text -> a -> Maybe Tags -> IO()
guage = gauge

-- | Send a counter.
counter :: (MetricData a) => T.Text -> a -> Maybe Tags -> Maybe Float -> IO()
counter = metric "c"

-- | Send a timer.
timer :: (MetricData a) => T.Text -> a -> Maybe Tags -> Maybe Float -> IO()
timer = metric "ms"

-- | Send a metric. Parses the options together. This function makes a
--   best-effort to send the metric; no metric-sending exceptions will be
--   thrown. The metric won't be sent if 'setupPickle' hasn't been called yet.
metric :: (MetricData a)
      => T.Text      -- ^ metric kind in character form (g,c,ms,s)
      -> T.Text      -- ^ metric name
      -> a           -- ^ metric value
      -> Maybe Tags  -- ^ Tags for metric
      -> Maybe Float -- ^ Sampling rate for applicable metrics.
      -> IO ()
metric kind n val mTags mSampling = do
    mPick <- gpPickle <$> atomically (readTVar pickle)
    case mPick of
        Nothing -> pure () -- no connection, give up.
        Just (Pickle sock cfg) -> do
            let tags = parseTags $ (fromMaybe M.empty mTags) <> (statsdTags cfg)
                sampling = maybe "" (\s -> "|@" <> showT s ) mSampling
                name = (statsdPrefix cfg) <> n
                msg  = name <> ":" <> (showT val) <> "|" <> kind <> sampling
            when (statsdVerbose cfg) (T.putStrLn $ "Sending metric: " <> msg)
            void $ (try $ send sock $ T.encodeUtf8 msg :: IO(Either SomeException Int))

-- | Parse tags into string to send.
parseTags :: Tags -> T.Text
parseTags tags
    | M.null tags = ""
    | otherwise   = parsed where
        parsed  = "#|" <> trimmed
        trimmed = T.dropEnd 1 catted
        catted  = M.foldrWithKey (\k a b -> b <> k <> ":" <> a <> ",") "" tags

data GlobalPickle = GlobalPickle {
    gpSetupRunning :: Bool
  , gpPickle       :: Maybe Pickle
  }

-- | Internal TVar keeping track of singleton connection.
pickle :: TVar GlobalPickle
pickle = unsafePerformIO $ newTVarIO $ GlobalPickle False Nothing
{-# NOINLINE pickle #-}

-- | Start the connection for our Pickle
initPickle :: StatsDConfig -> IO Pickle
initPickle cfg = do
    when (statsdVerbose cfg) $ putStrLn "Initializing Pickle StatsD Client.."
    addrinfos <- getAddrInfo Nothing (Just $ T.unpack $ statsdHost cfg) (Just $ T.unpack $ statsdPort cfg)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    connect sock (addrAddress serveraddr)
    pure $ Pickle sock cfg

-- | Internal utility to show something as Text
showT :: (Show a) => a -> T.Text
showT = T.pack . show
