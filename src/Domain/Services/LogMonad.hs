module Domain.Services.LogMonad where

import ClassyPrelude
  ( Bool
  , FilePath
  , IO
  , LazySequence(toStrict)
  , Monad
  , Ord((>=))
  , Text
  , ($)
  , (++)
  , (.)
  , getCurrentTime
  , putStrLn
  , unpack
  , when
  )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Time (formatISODateTime)
import Domain.Types.ImportTypes
  ( ErrorServer
  , LogConfig(LogConfig)
  , LogLevel
  , errorText
  )
import System.IO (appendFile)
import System.IO.Unsafe (unsafePerformIO)

class (Monad m) =>
      Log m
  where
  writeLog :: LogLevel -> Text -> m ()

toStrict1 :: BL.ByteString -> B.ByteString
toStrict1 = B.concat . BL.toChunks

writeInLogFile :: FilePath -> Bool -> Text -> IO ()
writeInLogFile lF bl txtLogF = do
  when bl $ appendFile lF (ClassyPrelude.unpack txtLogF)

writeInTerminal :: Bool -> Text -> IO ()
writeInTerminal bl txtTerm = do
  when bl $ ClassyPrelude.putStrLn txtTerm

writFileHandler :: FilePath -> LogLevel -> LogLevel -> Bool -> Text -> IO ()
writFileHandler lF logCong logToCompare bl txtLog = do
  writeInLogFile lF (logCong >= logToCompare) txtLog
  writeInTerminal bl txtLog

writeTextError :: ErrorServer -> Text -> Text
writeTextError err txtDesc = date ++ errText ++ txtDesc -- txtDesc - text description
  where
    date = takeCurrentDate
    errText = errorText err

writeText :: Text -> Text
writeText txtLog = date ++ txtLog
  where
    date = takeCurrentDate

takeCurrentDate :: Text
takeCurrentDate = toStrict $ formatISODateTime $ unsafePerformIO time
  where
    time = getCurrentTime

writeLogginHandler :: LogConfig -> LogLevel -> Text -> IO ()
writeLogginHandler (LogConfig lf logLev logBool) loging =
  writFileHandler lf logLev loging logBool
