{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Crawl.LevelInfo where

import Control.Applicative (liftA2)
import Control.Monad (mplus, when)
import Control.Monad.Trans.State (execState, put)
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F
import Control.Lens ((^?), (^..), (.=), (%~), _1, traverse, at, enum, contains, to, use)
import Control.Lens.TH (makeLenses)
import Data.Bits.Lens (bitAt)
import Numeric.Lens (integral)
import Control.Lens.Aeson (_Bool, _Array, _Integer, _String, key)
import qualified Data.Aeson as A
import qualified Data.Hashable as H
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Reactive.Banana as R

import Crawl.Bindings

type MapCell = Feature

data Coord = Coord !Int !Int deriving (Eq, Ord)
instance H.Hashable Coord where
  hash (Coord x y) = x + 100 * y

-- Want these to be strict, but how to catch uninitialized fields then?
data Monster = Monster {
  _monsterType :: MonsterType
  }

makeLenses ''Monster

uninitializedMonster :: Monster
uninitializedMonster = Monster (error "used uninitializedMonster monsterType!")

data LevelInfo = LevelInfo {
  _levelMap :: !(H.HashMap Coord MapCell),
  _levelFringe :: !(HS.HashSet Coord),
  _levelLOS :: !(HS.HashSet Coord),
  _levelMonsters :: !(H.HashMap Coord Monster),
  _levelMonsterTable :: !(H.HashMap Int Monster),
  _levelItemTiles :: !(H.HashMap Coord Int) -- todo: use an enum?
  }

makeLenses ''LevelInfo

emptyLevel :: LevelInfo
emptyLevel = LevelInfo H.empty HS.empty HS.empty H.empty H.empty H.empty

levelInfo :: R.Event t A.Value -> R.Behavior t LevelInfo
levelInfo input = R.accumB emptyLevel $ fmap (execState . updateLevel) input
  where updateLevel msg = when (msg ^? key "clear"._Bool == Just True) (put emptyLevel) >> mapM_ updateCell cellMsgs
          where cellMsgs = withCoordinates $ msg ^.. key "cells"._Array.traverse
                updateCell (coord, cellMsg) = do
                  F.mapM_ (\feat -> do
                              when (feat == DNGN_UNSEEN) (error "server set dungeon feature to DNGN_UNSEEN")
                              oldLevel <- use levelMap
                              levelMap.at coord .= Just feat
                              level <- use levelMap
                              let updateFringe c@(Coord x y) =
                                    levelFringe.contains c .=
                                    (not (c `H.member` level)
                                     && any (`H.member` level) [ Coord (x+dx) (y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], not (dx == 0 && dy == 0) ])
                              when (not $ coord `H.member` oldLevel) $ do -- neighboring area needs update
                                mapM_ updateFringe [ Coord (x+dx) (y+dy) | let Coord x y = coord, dx <- [-1,0,1], dy <- [-1,0,1] ])
                    (cellMsg ^? key "f"._Integer.integral.enum)
                  F.mapM_ (levelLOS.contains coord .=) (cellMsg ^? key "t".key "bg"._Integer.bitAt 18.to not)
                  F.mapM_ updateMonster (cellMsg ^? key "mon")

                  -- Update last seen item tile on this square, or remove if no item.
                  -- First decide whether the square has an item, doesn't have an item,
                  -- or has a monster so that we can't tell (then don't update).
                  -- It would probably be better to do this based on tile data
                  -- (can see under clouds, for instance) but tiles are confusing.
                  case cellMsg ^? key "g"._String of
                    Just g | g `T.isInfixOf` ".'" -> levelItemTiles.at coord .= Nothing
                    Just g | g `T.isInfixOf` ")([/%?\"=!:|$" ->
                      -- omitting {, } for now because disturbances/fountains share the glyphs
                      levelItemTiles.at coord .= cellMsg ^? key "t".key "fg"._Integer.integral
                    _ -> return ()
                  where updateMonster monsterData =
                          -- This crazy logic is intended to duplicate merge_monster
                          -- from webserver/game_data/static/map_knowledge.js
                          -- todo: garbage collect levelMonsterTable
                          case monsterData of
                            A.Null -> levelMonsters.at coord .= Nothing
                            _ -> do
                              let monsterID = monsterData ^? key "id"._Integer.integral
                              oldMonster <- liftA2 (\x y -> fromMaybe uninitializedMonster $ mplus x y)
                                            (maybe (return Nothing) (\mid -> use (levelMonsterTable.at mid)) monsterID)
                                            (use (levelMonsters.at coord))
                              let newMonster = (`execState` oldMonster) $ do
                                    F.mapM_ (monsterType .=) (monsterData ^? key "type"._Integer.integral.enum)
                              levelMonsters.at coord .= Just newMonster
                              F.forM_ monsterID $ \mid -> levelMonsterTable.at mid .= Just newMonster

withCoordinates :: [A.Value] -> [(Coord, A.Value)]
withCoordinates vs = map (_1 %~ (\(Just x, Just y) -> Coord x y)) $ scanl1 f $ map extractCoordinates vs
  where f ((ox, oy), _) ((nx, ny), nc) = ((nx', ny'), nc)
          where nx' = nx `mplus` (fmap (+1) ox)
                ny' = ny `mplus` oy
        extractCoordinates v = ((v ^? key "x"._Integer.integral, v ^? key "y"._Integer.integral), v)
