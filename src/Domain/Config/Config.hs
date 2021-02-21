module Domain.Config.Config where

import qualified Adapter.PostgreSQL.Common     as Pos
import           ClassyPrelude

import           Domain.Config.ParseConfig      ( ConfigPair
                                                , myParser
                                                )
import           Domain.Types.ExportTypes       ( ErrorServer(ErrorGetConfig) )
import qualified Domain.Types.LogEntity.LogEntity
                                               as Log
import qualified Prelude                       as P
import qualified Text.Parsec                   as Pars

data Config = Config
  { configPort :: Int
  , configLog  :: Log.StateLog
  , configPG   :: Pos.Config
  }
  deriving (Generic, Show)

parseConf :: Text -> IO (Either ErrorServer Config)
parseConf = return . configVKwithPair . getPairFromFile

charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

getPairFromFile :: Text -> Either Pars.ParseError [ConfigPair]
getPairFromFile = Pars.parse myParser ""

configVKwithPair
  :: Either Pars.ParseError [ConfigPair] -> Either ErrorServer Config
configVKwithPair (Left  _         ) = Left ErrorGetConfig
configVKwithPair (Right configPair) = do
      Right Config
        { configPort = P.read $ fromMaybe "3000" port
        , configLog  = Log.StateLog
                         { Log.logStCong = Log.LogConfig
                                             { Log.logFile = "log-journal"
                                             , Log.logLevelForFile = Log.Debug
                                             , Log.logConsole = True
                                             }
                         }
        , configPG   = Pos.Config
                         { Pos.configUrl = pack $ fmap charToWord8 $ fromMaybe
                                             "postgres"
                                             postgresOption
                         , Pos.configStripeCount          = 2
                         , Pos.configMaxOpenConnPerStripe = 5
                         , Pos.configIdleConnTimeout      = 10
                         }
        }
 where
  postgresOption = lookup "postgres" configPair
  port           = lookup "port" configPair
