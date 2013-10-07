{-# LANGUAGE OverloadedStrings #-}

module Crawl.Play (
  play
  ) where

import Control.Monad (forever, guard)
import Data.Maybe (fromMaybe)
import System.Exit (exitWith, ExitCode(ExitSuccess))

import Control.Concurrent.Chan.Split (InChan, OutChan, writeChan, readChan)
import Control.Lens ((^..), (^?), at, to, traverse, traverseAt)
import Data.Bits.Lens (bitAt)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H
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
    mapM_ recvFire $ demultiplex msg
    
setupNetwork :: R.Frameworks t => R.AddHandler A.Object -> (A.Object -> IO ()) -> R.Moment t ()
setupNetwork recvHandler sendHandler = do
  input <- R.fromAddHandler recvHandler
  let demultiplexed = input

      ping = R.filterE (\msg -> msg ^? traverseAt "msg" == Just "ping") demultiplexed
      pong = fmap (const $ H.fromList [("msg", "pong")]) ping

      messages = messagesOf demultiplexed

      inputModeEvents = filterBy (\msg -> do
                                     guard $ msg ^? traverseAt "msg" == Just "input_mode"
                                     msg ^? traverseAt "mode".asInteger) demultiplexed
      inputModeB = R.stepper 0 inputModeEvents
      inputModeChanged = R.filterApply (fmap (/=) inputModeB) inputModeEvents

      ourTurn = R.filterE (== 1) inputModeChanged
      forceMore = R.filterE (== 5) inputModeChanged

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
      output = pong `R.union` fmap textInput outputText
  R.reactimate $ fmap T.putStrLn messages
  R.reactimate $ fmap print outputText
  R.reactimate $ fmap sendHandler output
  R.reactimate $ fmap (const $ exitWith ExitSuccess) gameOver

demultiplex :: A.Object -> [A.Object]
demultiplex msg = {- if null subs then [msg] else -} subs
  where subs = msg^..at "msgs".traverse.asArray.traverse.asObject

-- XXX also have access to channel number, turn count
messagesOf :: R.Event t A.Object -> R.Event t T.Text
messagesOf input = R.spill $
                   filterBy (\msg -> do
                                guard  $ msg ^? traverseAt "msg".asString == Just "msgs"
                                let old_msgs = fromMaybe 0 $ msg ^? traverseAt "old_msgs".asInteger
                                return $ drop (fromInteger old_msgs)
                                  (msg ^.. traverseAt "messages".asArray.traverse.asObject.traverseAt "text".asString)) $
                   input

textInput :: T.Text -> A.Object
textInput text = H.fromList [
  ("msg", "input"),
  ("text", A.String text)
  ]
