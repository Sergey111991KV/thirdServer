module Domain.Types.LogEntity.LogEntity where

import ClassyPrelude
  ( Bool(..)
  , Eq((==))
  , FilePath
  , Generic
  , Ord(compare)
  , Ordering(EQ, GT, LT)
  , Read
  , Show
  )

newtype StateLog =
  StateLog
    { logStCong :: LogConfig
    }
  deriving (Generic, Show)

data LogConfig =
  LogConfig
    { logFile :: FilePath
    , logLevelForFile :: LogLevel
    , logConsole :: Bool
    }
  deriving (Show, Generic)

data LogLevel
  = Debug
  | Warning
  | ErrorLog
  deriving (Read, Show, Generic)

instance Eq LogLevel where
  (==) Debug Debug = True
  (==) Debug Warning = False
  (==) Debug ErrorLog = False
  (==) Warning Warning = True
  (==) Warning ErrorLog = False
  (==) ErrorLog ErrorLog = True
  (==) Warning Debug = False
  (==) ErrorLog Debug = False
  (==) ErrorLog Warning = False

instance Ord LogLevel where
  compare Debug Debug = EQ
  compare Debug Warning = LT
  compare Debug ErrorLog = LT
  compare Warning Warning = EQ
  compare Warning Debug = GT
  compare Warning ErrorLog = LT
  compare ErrorLog ErrorLog = EQ
  compare ErrorLog Debug = GT
  compare ErrorLog Warning = GT
