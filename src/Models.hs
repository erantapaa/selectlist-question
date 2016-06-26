{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Models where

import Data.Aeson                  (ToJSON, FromJSON)
import Data.Int                    (Int64)
import Data.Time                   (UTCTime, LocalTime, getCurrentTimeZone, utcToLocalTime)
import GHC.Generics                (Generic)
import Control.Monad.Reader        (Reader, ReaderT, asks, liftIO)
import Control.Monad.Trans         (MonadIO)
import Database.Persist.Postgresql
import Database.Persist.TH         (share, mkPersist, sqlSettings,
                                    mkMigrate, persistLowerCase)

import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Reading json
    diastolic Int
    systolic Int
    timestamp UTCTime default=now()
|]

data PReading = PReading {
  diastolic :: Int
  , systolic :: Int
  , datetime :: LocalTime
  } deriving (Eq, Show, Generic)

instance ToJSON PReading
instance FromJSON PReading

readingOutput :: Reading -> IO PReading
readingOutput Reading{..} = do
  tz <- getCurrentTimeZone
  return $ PReading {
    diastolic = readingDiastolic
    , systolic = readingSystolic
    , datetime = utcToLocalTime tz readingTimestamp}



doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
