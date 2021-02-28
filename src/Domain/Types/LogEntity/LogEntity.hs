module Domain.Types.LogEntity.LogEntity where


import           ClassyPrelude                  ( Eq
                                                , Ord
                                                , Read
                                                , Show
                                                , Generic
                                                , FilePath
                                                )

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )

newtype StateLog =
  StateLog
    { logStCong :: LogConfig
    }
  deriving (Generic, Show)


data LogConfig = LogConfig
  { logFile         :: FilePath
  , logLevel        :: LogLevel
  }
  deriving (Show, Generic)


data LogLevel
  = Debug
  | Warning
  | Error
  deriving (Eq, Ord, Read, Show, Generic)


instance ToJSON LogLevel

instance FromJSON LogLevel

instance ToJSON LogConfig

instance FromJSON LogConfig

