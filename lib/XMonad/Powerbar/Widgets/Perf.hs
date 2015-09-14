{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module XMonad.Powerbar.Widgets.Perf
    ( perfWidget
    , perfQuery
      -- CPU
    , CpuStats
    , cpuIdle
    , cpuActive
    , cpuLoad
    , getCpuStats
    -- Memory
    , getMemoryStats
    , memLoad
    -- IO
    , getIOStats
    , IOStats
    , ioLoad
    ) where

import XMonad.Powerbar.View
import XMonad.Powerbar.Style
import XMonad.Powerbar.Lens
import XMonad.Powerbar.Widgets.Common
import XMonad.Powerbar.Widgets.Icon (iconCell, iconBar)
import qualified XMonad as X

import Control.Applicative    (many)
import Control.Monad          (foldM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Monoid            ((<>))
import Data.Typeable          (Typeable)
import Data.Time.Clock.POSIX  (POSIXTime, getPOSIXTime)
import System.IO              (IOMode(..), withBinaryFile)

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Map as M


--------------------------------------------------------------------------------
-- CPU statistics
--
data CpuStats = CpuStats { _cpuIdle   :: Int
                         , _cpuActive :: Int
                         } deriving (Show, Typeable)

cpuIdle :: Lens' CpuStats Int
cpuIdle f v = (\c -> v {_cpuIdle = c}) `fmap` f (_cpuIdle v)

cpuActive :: Lens' CpuStats Int
cpuActive f v = (\c -> v {_cpuActive = c}) `fmap` f (_cpuActive v)


-- | Calculate cpu load
cpuLoad :: CpuStats -> CpuStats -> Float
cpuLoad s s' = (1/) . (+1) $ (idle s' - idle s) / (active s' - active s)
    where idle   = fromIntegral . view cpuIdle
          active = fromIntegral . view cpuActive


-- | Get cpu statistics
getCpuStats :: MonadIO m => m (Maybe CpuStats)
getCpuStats = do
  -- ByteString's readFile is broken as proc filesystem reports zero size for file
  stats <- liftIO . withBinaryFile "/proc/stat" ReadMode $ C.hGetContents
  return . either (const Nothing) Just . A.parseOnly parseStats $ stats
      where
        parseStats :: A.Parser CpuStats
        parseStats = do
                A.string "cpu"
                A.skipSpace
                vals <- A.sepBy A.decimal A.skipSpace
                -- user nice system idle iowait irq softirq steal guest guest_nice
                -- Idle   = idle + iowait
                -- Active = user + nice + system + irq + softirq + steal
                return $ foldl (flip ($)) (CpuStats 0 0)
                       $ zipWith ($) [ (cpuActive +~) -- user
                                     , (cpuActive +~) -- nice
                                     , (cpuActive +~) -- system
                                     , (cpuIdle   +~) -- idle
                                     , (cpuIdle   +~) -- iowait
                                     , (cpuActive +~) -- irq
                                     , (cpuActive +~) -- softirq
                                     , (cpuActive +~) -- steal
                                     ] vals


--------------------------------------------------------------------------------
-- Memory statistics
--
type MemoryStats = M.Map C.ByteString Int

-- | Get memory statistics
getMemoryStats :: MonadIO m => m (Maybe MemoryStats)
getMemoryStats = do
  -- ByteString's readFile is broken as proc filesystem reports zero size for file
  stats <- liftIO . withBinaryFile "/proc/meminfo" ReadMode $ C.hGetContents
  return . either (const Nothing) Just . A.parseOnly parseStats $ stats
      where parseStats :: A.Parser MemoryStats
            parseStats = fmap M.fromList $ A.many' $ do
                           name <- A.takeTill (== ':')
                           A.char ':'
                           A.skipSpace
                           value <- A.decimal
                           A.skipWhile (/= '\n') >> A.anyChar
                           return (name, value)


-- | Memory load
memLoad :: MemoryStats -> Maybe Float
memLoad stats = do a <- fromIntegral <$> avail
                   t <- fromIntegral <$> total
                   return $ (t - a) / t
    where
      -- avail = MemFree + Buffers + Cached
      avail = foldM (\v n -> (+v) `fmap` M.lookup n stats) 0
              ["MemFree", "Buffers", "Cached"]
      -- total = MemTotal
      total = M.lookup "MemTotal" stats


--------------------------------------------------------------------------------
-- IO Stats
--
data IOStats = IOStats { _ioStamp :: POSIXTime
                       , _ioTicks :: M.Map String Int  -- Partition -> IO_Ticks
                       } deriving (Show, Typeable)

ioTicks :: Lens' IOStats (M.Map String Int)
ioTicks f v = (\c -> v {_ioTicks = c}) `fmap` f (_ioTicks v)

ioStamp :: Lens' IOStats POSIXTime
ioStamp f v = (\c -> v {_ioStamp = c}) `fmap` f (_ioStamp v)


-- | Find device io with maximum load (io_ticks / time)
ioLoad :: IOStats -> IOStats -> (String, Float)
ioLoad s s' = (name, fromIntegral ticks / 1000 / realToFrac (s' ^. ioStamp - s ^. ioStamp))
    where
      -- most loaded device
      (name, ticks) = foldl (\(n,v) (n',v') -> if v > v' then (n,v) else (n',v')) ("", 0)
                      $ do (n, v) <- s  ^.. ioTicks . to M.toList . traverse
                           v'     <- s' ^.. ioTicks . at n . _Just
                           return (n, v' - v)


getIOStats :: MonadIO m => m (Maybe IOStats)
getIOStats = do
  stats <- liftIO . withBinaryFile "/proc/diskstats" ReadMode $ C.hGetContents
  now   <- liftIO getPOSIXTime
  return . either (const Nothing) Just $ A.parseOnly (parseStats now) stats
      where word = A.takeTill A.isSpace <* A.skipSpace
            parseStats :: POSIXTime -> A.Parser IOStats
            parseStats now = fmap (IOStats now . M.fromList) $ many $ do
               A.skipSpace
               A.count 2 word            -- device major and minor values
               name <- C.unpack <$> word  -- device name
               -- Skip:
               --   read, read-merge, read-sectors, read-ms
               --   write, write-merge, write-sectors, write-ms
               --   curret-size-io-queue
               A.count 9 word
               io_ticks <- A.decimal <* A.skipSpace
               word                      -- time_in_queue
               A.skipWhile (== '\n')
               return (name, io_ticks)


--------------------------------------------------------------------------------
-- Performance widget
--

-- | Performance statistics widget
perfWidget :: X.X View
perfWidget =
    widget "Perf" 5 perfQuery (CpuStats 0 0, IOStats 0 M.empty) $ do
      -- cpu stats
      cs <- widgetMaybeIO getCpuStats
      cl <- _1 %%= \cs' -> (cpuLoad cs' cs, cs)
      -- io stats
      is <- widgetMaybeIO getIOStats
      il <- _2 %%= \is' -> (ioLoad is' is, is)
      -- mem stats
      ml <- widgetMaybeIO $ (memLoad =<<) <$> getMemoryStats
      -- aggregated stats
      let stats = J.object ["cpu" J..= cl, "mem" J..= ml, "io"  J..= il]
      -- view
      return . VAction VButtonLeft (asks (X.terminal . X.config) >>= X.spawn  . (++" -e htop"))
             . VAttr ".perf" stats
             $  VAttr ".perf-icon" Null VEmpty
             <> VAttr ".perf-cpu" (toJSON cl) VEmpty
             <> VAttr ".perf-mem" (toJSON ml) VEmpty
             <> VAttr ".perf-io"  (toJSON il) (VText . CL.unpack . J.encode $ stats)


-- | Performance widget default style query
perfQuery :: StyleQuery ()
perfQuery = applyAll [ do matchAttr ".perf-icon"
                          fontSize += 2
                          iconCell "octicon-dashboard"
                     , do cl <- matchAttr' ".perf-cpu" _Float
                          colorFg .= redL
                          iconBar cl
                     , do ml <- matchAttr' ".perf-mem" _Float
                          colorFg .= greenL
                          iconBar ml
                     , do il <- matchAttr' ".perf-io" _JSON :: StyleQuery (String, Float)
                          colorFg .= blueL
                          iconBar . snd $ il
                     ]
