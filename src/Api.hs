{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Either (EitherT)
import Data.Int (Int64)
import Servant
import Config (Config(..))
import Models

type ReadingApi = "reads" :> Get '[JSON] [PReading]

type AppM = ReaderT Config (EitherT ServantErr IO)

readingApi :: Proxy ReadingApi
readingApi = Proxy

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg
