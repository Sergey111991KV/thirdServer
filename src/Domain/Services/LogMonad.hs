module Domain.Services.LogMonad where

import ClassyPrelude


import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Time (formatISODateTime)
import Domain.Types.ImportTypes
 


import Data.Text.Time (formatISODateTime)
import System.IO (appendFile)

class (Monad m, MonadIO m) =>
      Log m
  where
  writeLog :: LogWrite -> Text -> m ()
  writeLogE :: Text -> m ()
  writeLogW :: Text -> m ()
  writeLogD :: Text -> m ()

writeInLogFile :: FilePath -> Bool -> Text -> IO ()
writeInLogFile lF bl txtInLog = do
  when bl $ appendFile lF (ClassyPrelude.unpack txtInLog)

writeInTerminal :: Bool -> Text -> IO ()
writeInTerminal bl txtInLog = do
  when bl $ ClassyPrelude.putStrLn txtInLog

writFileHandler ::
     UTCTime -> FilePath -> LogWrite -> LogWrite -> Bool -> Text -> IO ()
writFileHandler dat lF logConf logToCompare bl txtInLog = do
  writeInLogFile lF (logConf >= logToCompare) (txtInLog <> " " <> d)
  writeInTerminal bl txtInLog
  where
    d = toStrict $ formatISODateTime dat

writeLogHandler :: UTCTime -> LogConfig -> LogWrite -> Text -> IO ()
writeLogHandler dat (LogConfig lf logLev logBool) loging =
  writFileHandler dat lf logLev loging logBool