{-# LANGUAGE OverloadedStrings #-}

module Crawl.Play (
  play
  ) where

import Control.Monad (forever, guard)
import System.Exit (exitWith, ExitCode(ExitSuccess))

import Control.Concurrent.Chan.Split (InChan, OutChan, writeChan, readChan)
import Control.Lens ((^.), (^..), (^?), (.~), at, to, traverse, traverseAt,
                     withIndicesOf, itraversed, (<.))
import Data.Bits.Lens (bitAt)
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

      (_messageArea, messageUpdates) = txtArea "messages" demultiplexed
      messages = R.spill $ fmap (\area -> filter (not . T.null) $ IM.elems area) messageUpdates

      ourTurn = R.filterE (\msg -> msg ^? traverseAt "msg" == Just "input_mode" &&
                                   msg ^? traverseAt "mode".asInteger == Just 1) demultiplexed
      forceMore = R.filterE (T.isInfixOf "--more--") messages

      inventoryMore = R.filterE (\msg -> msg ^? traverseAt "msg" == Just "menu" &&
                                         msg ^? traverseAt "tag" == Just "inventory" &&
                                         msg ^? traverseAt "flags" . asInteger . bitAt 12 == Just True)
                      demultiplexed

      goodbye = R.filterE (\msg -> msg ^? traverseAt "msg" == Just "txt" &&
                                   msg ^? traverseAt "id"  == Just "crt" &&
                                   msg ^? traverseAt "lines".asObject.traverseAt "0".asString.to (T.isInfixOf "Goodbye,")
                                   == Just True
                          ) demultiplexed

      gameOver = R.filterE (\msg -> msg ^? traverseAt "msg" == Just "game_ended") demultiplexed

      goText = fmap (T.singleton . ("hjklyubn" !!)) $ randomize (0,7) ourTurn
      clearText = fmap (const " ") forceMore `R.union`
                  fmap (const " ") inventoryMore `R.union`
                  fmap (const " ") goodbye

      outputText = goText `R.union` clearText
      output = fmap textInput outputText
  R.reactimate $ fmap T.putStrLn messages
  R.reactimate $ fmap print outputText
  R.reactimate $ fmap sendHandler output
  R.reactimate $ fmap (const $ exitWith ExitSuccess) gameOver

demultiplex :: A.Object -> [A.Object]
demultiplex msg
  | Just "multi" == msg^.at "msg" =
    concatMap demultiplex (msg^..at "msgs".traverse.asArray.traverse.asObject)
  | otherwise = [msg]

-- XXX what to do about incremental updates?
txtArea :: T.Text -> R.Event t A.Object -> (R.Behavior t (IM.IntMap T.Text), R.Event t (IM.IntMap T.Text))
txtArea txtID input = (areaB, R.filterApply (fmap (/=) areaB) areaE)
  where areaUpdates = fmap (foldr (.) id . map (\(k, v) -> at (read $ T.unpack k) .~ Just v)) .
                      filterBy (\msg -> do
                                   guard  $ msg ^? traverseAt "msg".asString == Just "txt"
                                     &&     msg ^? traverseAt "id".asString == Just txtID
                                   return $ msg ^.. traverseAt "lines".asObject.
                                     withIndicesOf (itraversed <. asString)
                               ) $
                      input
        -- XXX use mapAccum
        areaB = R.accumB IM.empty areaUpdates
        areaE = R.accumE IM.empty areaUpdates

textInput :: T.Text -> A.Object
textInput text = H.fromList [
  ("msg", "input"),
  ("text", A.String text)
  ]
