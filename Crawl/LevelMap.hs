{-# LANGUAGE OverloadedStrings #-}

module Crawl.LevelMap where

import Control.Monad (mplus)
import Control.Lens ((^?), (^..), (.~), (%~), _1, traverse, at, enum)
import Numeric.Lens (integral)
import Control.Lens.Aeson (_Bool, _Array, _Integer, key)
import qualified Data.Aeson as A
import qualified Data.Hashable as H
import qualified Data.HashMap.Strict as H
import qualified Reactive.Banana as R

import Crawl.Bindings

type MapCell = Feature

data Coord = Coord !Int !Int deriving (Eq, Ord)
instance H.Hashable Coord where
  hash (Coord x y) = x + 100 * y

type LevelMap = H.HashMap Coord MapCell

levelMap :: R.Event t A.Value -> R.Behavior t LevelMap
levelMap input = R.accumB H.empty $ fmap updateMap input
  where updateMap msg = foldr (.) base (map updateCell cellMsgs)
          where base = case msg ^? key "clear"._Bool of
                  Just True -> const H.empty
                  _ -> id
                cellMsgs = withCoordinates $ msg ^.. key "cells"._Array.traverse
                updateCell (coord, cellMsg) = maybe id ((at coord .~) . Just) $ cellMsg ^? key "f"._Integer.integral.enum

withCoordinates :: [A.Value] -> [(Coord, A.Value)]
withCoordinates vs = map (_1 %~ (\(Just x, Just y) -> Coord x y)) $ scanl1 f $ map extractCoordinates vs
  where f ((ox, oy), _) ((nx, ny), nc) = ((nx', ny'), nc)
          where nx' = nx `mplus` (fmap (+1) ox)
                ny' = ny `mplus` oy
        extractCoordinates v = ((v ^? key "x"._Integer.integral, v ^? key "y"._Integer.integral), v)
