{-# LANGUAGE OverloadedStrings #-}

module Crawl.Play (
  play
  ) where

import Control.Monad (forever, guard)

import Control.Concurrent.Chan.Split (InChan, OutChan, writeChan, readChan)
import Control.Lens ((^.), (^..), (^?), (.~), at, traverse, traverseAt,
                     withIndicesOf, itraversed, (<.))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H
import qualified Data.IntMap as IM
import qualified Reactive.Banana as R
import qualified Reactive.Banana.Frameworks as R
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Crawl.AesonLens
import Crawl.BananaUtils

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

      messageArea = txtArea "messages" demultiplexed
      messageUpdates = actualChanges messageArea input
      messages = R.spill $ fmap (\area -> filter (not . T.null) $ IM.elems area) messageUpdates

      output = bananaCrawl demultiplexed
  R.reactimate $ fmap T.putStrLn messages
  R.reactimate $ fmap sendHandler output

demultiplex :: A.Object -> [A.Object]
demultiplex msg
  | Just "multi" == msg^.at "msg" =
    concatMap demultiplex (msg^..at "msgs".traverse.asArray.traverse.asObject)
  | otherwise = [msg]

txtArea :: T.Text -> R.Event t A.Object -> R.Behavior t (IM.IntMap T.Text)
txtArea txtID input = area
  where areaUpdates = R.spill $
                      filterBy (\msg -> do
                                   guard  $ msg ^? traverseAt "msg".asString == Just "txt"
                                     &&     msg ^? traverseAt "id".asString == Just txtID
                                   return $ msg ^.. traverseAt "lines".asObject.
                                     withIndicesOf (itraversed <. asString)
                               ) input
        area = R.accumB IM.empty $ fmap (\(k, v) -> at (read $ T.unpack k) .~ Just v) areaUpdates

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
