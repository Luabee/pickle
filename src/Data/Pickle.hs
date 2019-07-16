{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Pickle ( Tags(..)
                   , StatsDConfig(..)
                   , MetricData
                   , defaultConfig
                   , withPickleDo
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
import Control.Concurrent.MVar

-- | Tags for DogStatsD
type Tags = M.Map T.Text T.Text

-- | Configuration for the UDP connection used
data StatsDConfig = StatsDConfig { statsdHost      :: T.Text -- ^ Host of statsd server
                                 , statsdPort      :: T.Text -- ^ Port of statsd server
                                 , statsdPrefix    :: T.Text -- ^ Prefix concatenated to all metrics names in our program
                                 , statsdTags      :: Tags   -- ^ Mappended tags for all stats we report
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

{-| Start up our statsd client. This can and should be attached directly to main:
> main = withPickleDo defaultConfig $ do (...)

This function can be nested, but one thread in your program at a time should be the "owner" of the pickle stack.
Other threads can use the active pickle, but they shouldn't call this function since it changes settings for all threads.
-}
withPickleDo :: StatsDConfig -> IO a -> IO a
withPickleDo cfg f = do
    mPick <- catch (Just <$> takeMVar pickle) (\(e :: BlockedIndefinitelyOnMVar) -> pure Nothing )
    case mPick of 
        Nothing -> do -- First initialization.
            pick <- initPickle cfg
            bracket_
                (putMVar pickle (pick))
                (close =<< pickleSock <$> takeMVar pickle)
                (f)
        Just oldPick -> do -- Update settings.
            newPick <- initPickle cfg
            bracket_
                (putMVar pickle (newPick))
                (do
                    close =<< pickleSock <$> takeMVar pickle
                    putMVar pickle oldPick
                    )
                (f)

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

-- | Send a metric. Parses the options together.
metric :: (MetricData a)
      => T.Text      -- ^ metric kind in character form (g,c,ms,s)
      -> T.Text      -- ^ metric name
      -> a           -- ^ metric value
      -> Maybe Tags  -- ^ Tags for metric
      -> Maybe Float -- ^ Sampling rate for applicable metrics.
      -> IO()
metric kind n val mTags mSampling = do
    Pickle sock cfg <- takeMVar pickle
    let tags = parseTags $ (fromMaybe M.empty mTags) <> (statsdTags cfg)
        sampling = maybe "" (\s -> "|@" <> showT s ) mSampling
        name = (statsdPrefix cfg) <> n
        msg  = name <> ":" <> (showT val) <> "|" <> kind <> sampling
    when (statsdVerbose cfg) (T.putStrLn $ "Sending metric: " <> msg)
    void $ send sock $ T.encodeUtf8 msg 

-- | Parse tags into string to send.
parseTags :: Tags -> T.Text
parseTags tags 
    | M.null tags = ""
    | otherwise   = parsed where
        parsed  = "#|" <> trimmed
        trimmed = T.dropEnd 1 catted
        catted  = M.foldrWithKey (\k a b -> b <> k <> ":" <> a <> ",") "" tags

-- | Internal MVar keeping track of singleton connection.
pickle :: MVar Pickle
pickle = unsafePerformIO $ newEmptyMVar :: MVar Pickle
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