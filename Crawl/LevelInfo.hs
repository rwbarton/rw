{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Crawl.LevelInfo where

import Control.Monad (mplus, when)
import Control.Monad.Trans.State (execState, put)
import qualified Data.Foldable as F
import Control.Lens ((^?), (^..), (.=), (%~), _1, traverse, at, enum, contains)
import Control.Lens.TH (makeLenses)
import Numeric.Lens (integral)
import Control.Lens.Aeson (_Bool, _Array, _Integer, key)
import qualified Data.Aeson as A
import qualified Data.Hashable as H
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Reactive.Banana as R

import Crawl.Bindings

type MapCell = Feature

data Coord = Coord !Int !Int deriving (Eq, Ord)
instance H.Hashable Coord where
  hash (Coord x y) = x + 100 * y

data LevelInfo = LevelInfo {
  _levelMap :: H.HashMap Coord MapCell,
  _levelMonsters :: HS.HashSet Coord
  }

makeLenses ''LevelInfo

emptyLevel :: LevelInfo
emptyLevel = LevelInfo H.empty HS.empty

levelInfo :: R.Event t A.Value -> R.Behavior t LevelInfo
levelInfo input = R.accumB emptyLevel $ fmap (execState . updateLevel) input
  where updateLevel msg = when (msg ^? key "clear"._Bool == Just True) (put emptyLevel) >> mapM_ updateCell cellMsgs
          where cellMsgs = withCoordinates $ msg ^.. key "cells"._Array.traverse
                updateCell (coord, cellMsg) = do
                  F.mapM_ ((levelMap.at coord .=) . Just) (cellMsg ^? key "f"._Integer.integral.enum)
                  F.mapM_ ((levelMonsters.contains coord .=) . (/= A.Null)) (cellMsg ^? key "mon")

withCoordinates :: [A.Value] -> [(Coord, A.Value)]
withCoordinates vs = map (_1 %~ (\(Just x, Just y) -> Coord x y)) $ scanl1 f $ map extractCoordinates vs
  where f ((ox, oy), _) ((nx, ny), nc) = ((nx', ny'), nc)
          where nx' = nx `mplus` (fmap (+1) ox)
                ny' = ny `mplus` oy
        extractCoordinates v = ((v ^? key "x"._Integer.integral, v ^? key "y"._Integer.integral), v)
