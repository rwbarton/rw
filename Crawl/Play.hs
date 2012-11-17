{-# LANGUAGE OverloadedStrings #-}

module Crawl.Play (
  play
  ) where

import Control.Monad (forever)

import Control.Concurrent.Chan.Split (InChan, OutChan, writeChan, readChan)
import Control.Lens ((^.), (^..), at, traverse)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H
import qualified Reactive.Banana as R
import qualified Reactive.Banana.Frameworks as R

import Crawl.AesonLens

play :: OutChan A.Object -> InChan A.Object -> IO ()
play recvChan sendChan = do
  (recvHandler, recvFire) <- R.newAddHandler
  let sendHandler = writeChan sendChan
  network <- R.compile $ setupNetwork recvHandler sendHandler
  R.actuate network
  forever $ do
    msg <- readChan recvChan
    recvFire msg
    
setupNetwork :: R.Frameworks t => R.AddHandler A.Object -> (A.Object -> IO ()) -> R.Moment t ()
setupNetwork recvHandler sendHandler = do
  input <- R.fromAddHandler recvHandler
  let demultiplexed = R.spill $ fmap demultiplex input
      output = bananaCrawl demultiplexed
  R.reactimate $ fmap sendHandler output

demultiplex :: A.Object -> [A.Object]
demultiplex msg
  | Just "multi" == msg^.at "msg" =
    concatMap demultiplex (msg^..at "msgs".traverse.asArray.traverse.asObject)
  | otherwise = [msg]

bananaCrawl :: R.Event t A.Object -> R.Event t A.Object
bananaCrawl input = fmap (const rest) input `R.union` fmap (const space) input
  where rest = H.fromList [
          ("msg", "input"),
          ("text", ".")
          ]
        space = H.fromList [
          ("msg", "input"),
          ("text", " ")
          ]
