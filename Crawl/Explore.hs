module Crawl.Explore (
  explore
  ) where

import Data.Graph.AStar (aStar)
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S

import Crawl.Bindings
import Crawl.LevelInfo
import Crawl.Move

explore :: LevelInfo -> Coord -> Maybe Move
explore info loc = case aStar adj cost bound isUnmapped loc of
  Just (loc' : _) -> Just (moveTo loc loc')
  _ -> Nothing
  where adj (Coord x y) = S.fromList [ target
                          | dx <- [-1,0,1], dy <- [-1,0,1], not (dx == 0 && dy == 0),
                            let target = Coord (x+dx) (y+dy),
                            maybe True isPassable (H.lookup target level) ]
        cost _ target = movementCost (level H.! target)
        bound _ = 0
        isUnmapped target = not $ target `H.member` level
        level = _levelMap info

moveTo :: Coord -> Coord -> Move
moveTo (Coord sx sy) (Coord tx ty) = Go (tx - sx) (ty - sy)

isPassable :: Feature -> Bool
isPassable feat | DNGN_MANGROVE <= feat && feat <= DNGN_DEEP_WATER = False
isPassable _ = True

movementCost :: Feature -> Int
movementCost _ = 1
