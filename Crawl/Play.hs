{-# LANGUAGE OverloadedStrings #-}

module Crawl.Play (
  play
  ) where

import Control.Monad (forever)

import Control.Concurrent.Chan.Split (InChan, OutChan, writeChan, readChan)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H

play :: OutChan A.Object -> InChan A.Object -> IO ()
play recvChan sendChan = forever $ do
  msg <- readChan recvChan
  writeChan sendChan $ H.fromList [
    ("msg", "input"),
    ("text", ".")
    ]
  writeChan sendChan $ H.fromList [
    ("msg", "input"),
    ("text", " ")
    ]
