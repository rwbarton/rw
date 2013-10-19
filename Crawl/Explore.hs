module Crawl.Explore (
  explore, loot, descend
  ) where

import Control.Monad (guard)

import Data.Graph.AStar (aStar)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Set as S

import Crawl.Bindings
import Crawl.LevelInfo
import Crawl.Move

pathfind :: (Coord -> Bool) -> Bool -> LevelInfo -> Coord -> Maybe [Coord]
pathfind isGoal moveThroughUnknown info loc = aStar adj cost bound isGoal loc
  where adj (Coord x y) = S.fromList [ target
                          | dx <- [-1,0,1], dy <- [-1,0,1], not (dx == 0 && dy == 0),
                            let target = Coord (x+dx) (y+dy),
                            maybe moveThroughUnknown isPassable (H.lookup target level) ]
        cost _ target = movementCost (level H.! target)
        bound _ = 0
        level = _levelMap info

explore :: LevelInfo -> Coord -> Maybe Move
explore info loc = case pathfind isUnmapped True info loc of
  Just (loc' : _) -> Just (moveTo loc loc')
  _ -> Nothing
  where isUnmapped target = not $ target `H.member` _levelMap info

loot :: LevelInfo -> Coord -> Maybe Move
loot info loc = guard (not $ HS.null (_levelLoot info)) >> case pathfind isLoot False info loc of
  Just (loc' : _) -> Just (moveTo loc loc')
  _ -> Nothing
  where isLoot target = target `HS.member` _levelLoot info

descend :: LevelInfo -> Coord -> Maybe Move
descend info loc = case pathfind (\coord -> fmap isDownStair (H.lookup coord (_levelMap info)) == Just True) False info loc of
  Just [] -> Just GoDown
  Just (loc' : _) -> Just (moveTo loc loc')
  _ -> Nothing
  where isDownStair DNGN_STONE_STAIRS_DOWN_I = True
        isDownStair DNGN_STONE_STAIRS_DOWN_II = True
        isDownStair DNGN_STONE_STAIRS_DOWN_III = True
        isDownStair DNGN_ESCAPE_HATCH_DOWN = True
        isDownStair DNGN_EXIT_PORTAL_VAULT = True
        isDownStair DNGN_EXIT_ABYSS = True
        isDownStair _ = False

moveTo :: Coord -> Coord -> Move
moveTo (Coord sx sy) (Coord tx ty) = Go (tx - sx) (ty - sy)

isPassable :: Feature -> Bool
isPassable feat | DNGN_MANGROVE <= feat && feat <= DNGN_DEEP_WATER = False
isPassable _ = True

movementCost :: Feature -> Int
movementCost _ = 1
