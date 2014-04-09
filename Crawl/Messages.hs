{-# LANGUAGE OverloadedStrings #-}

module Crawl.Messages (
  Message(..),
  messagesOf
  ) where

import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens ((^?), (^?!), (^..), enum, traverse)
import Numeric.Lens (integral)
import Data.Aeson.Lens (_Integer, _String, _Array, key)
import qualified Data.Aeson as A
import qualified Reactive.Banana as R
import qualified Data.Text as T

import Crawl.BananaUtils
import Crawl.Bindings

data Message = Message {
  _msgChannel :: !MsgChannel,
  _msgText :: !T.Text,
  _msgTurn :: !Int
  } deriving (Show)

-- XXX also have access to turn count
messagesOf :: R.Event t A.Value -> R.Event t Message
messagesOf input =
  R.spill $
  filterBy (\msg -> do
               guard $ msg ^? key "msg" == Just "msgs"
               let old_msgs = fromMaybe 0 $ msg ^? key "old_msgs"._Integer
                   parse message = Message
                                   (message ^?! key "channel"._Integer.integral.enum)
                                   (message ^?! key "text"._String)
                                   (message ^?! key "turn"._Integer.integral)
               return $ drop (fromInteger old_msgs) $ map parse $
                 (msg ^.. key "messages"._Array.traverse))
  input
