module Domain.Services.LogMonad where

import           ClassyPrelude                  ( ($)
                                                , Monad
                                                , Ord((<=))
                                                , Semigroup((<>))
                                                , IO
                                                , Text
                                                , UTCTime
                                                , when
                                                , FilePath
                                                , putStrLn
                                                , unpack
                                                , MonadIO
                                                , LazySequence(toStrict)
                                                )

import           Domain.Types.ExportTypes       ( LogConfig(logLevel, logFile)
                                                , LogLevel
                                                )
import           Data.Text.Time                 ( formatISODateTime )
import           System.IO                      ( appendFile )

class (Monad m, MonadIO m) =>
      Log m
  where
  writeLog  :: LogServer -> Text -> m ()
  writeLogE :: Text -> m ()
  writeLogW :: Text -> m ()
  writeLogD :: Text -> m ()

writeInLogFile :: FilePath -> Text -> IO ()
writeInLogFile lF txtInLog = appendFile lF (ClassyPrelude.unpack txtInLog)

type LogServer = LogLevel

writeLogHandler :: UTCTime -> LogConfig -> LogServer -> Text -> IO ()
writeLogHandler dat logConf logServ txt = when
  (logLevel logConf <= logServ)
  (do
    putStrLn txt
    writeInLogFile (logFile logConf) (txt <> " " <> d)
  )
  where d = toStrict $ formatISODateTime dat



