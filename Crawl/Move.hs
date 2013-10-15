{-# LANGUAGE OverloadedStrings #-}

module Crawl.Move (
  Move(..), moveToText
  ) where

import qualified Data.Text as T

data Move = Go !Int !Int
          | Rest

moveToText :: Move -> T.Text
moveToText (Go dx dy) = case (dx, dy) of
  (-1,  0) -> "h"
  ( 0,  1) -> "j"
  ( 0, -1) -> "k"
  ( 1,  0) -> "l"
  (-1, -1) -> "y"
  ( 1, -1) -> "u"
  (-1,  1) -> "b"
  ( 1,  1) -> "n"
  _        -> error "tried to make illegal move"
moveToText Rest = "."
