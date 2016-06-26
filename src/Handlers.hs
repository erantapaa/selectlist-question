{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Data.Aeson (toJSON)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (lift)
import Control.Monad.Trans.Either (left)
import Data.Time (UTCTime, getCurrentTimeZone, utcToLocalTime)
import Network.Wai (Application)
import Database.Persist.Postgresql (get, selectList, Entity(..),
                                    insert, (==.), toSqlKey, fromSqlKey)
import Data.Int (Int64)
import Servant
import Api
import Config (Config(..))
import Models


app :: Config -> Application
app cfg = serve readingApi (readerServer cfg)

readingServer :: ServerT ReadingApi AppM
readingServer = allReadings

readerServer :: Config -> Server ReadingApi
readerServer cfg = enter (readerToEither cfg) readingServer :: _


allReadings :: AppM [PReading]
allReadings = do
  readings <- runDb $ selectList [] []
  let results = map (\(Entity _ a) -> readingOutput a) readings
  liftIO $ sequence results
