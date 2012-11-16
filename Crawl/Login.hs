{-# LANGUAGE OverloadedStrings #-}

module Crawl.Login (
  login,
  sendRCFile,
  startGame
  ) where

import System.Process (system)

import Control.Concurrent.Chan.Split (InChan, writeChan)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H

import Crawl.AccountInfo

login :: InChan A.Object -> AccountInfo -> IO ()
login sendChan config = writeChan sendChan $ H.fromList [
  ("msg", A.String "login"),
  ("username", A.toJSON (username config)),
  ("password", A.toJSON (password config))
  ]

sendRCFile :: InChan A.Object -> AccountInfo -> IO ()
sendRCFile sendChan config = do
  system "make rw.rc"
  rc <- readFile "rw.rc"
  writeChan sendChan $ H.fromList [
    ("msg", A.String "set_rc"),
    ("game_id", A.toJSON (game_id config)),
    ("contents", A.toJSON rc)
    ]

-- XXX probably makes sense to consume all messages up to game_started
startGame :: InChan A.Object -> AccountInfo -> IO ()
startGame sendChan config = writeChan sendChan $ H.fromList [
  ("msg", A.String "play"),
  ("game_id", A.toJSON (game_id config))
  ]