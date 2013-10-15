module Crawl.Explore (
  explore
  ) where

import Data.Graph.AStar (aStar)
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Text as T

import Crawl.Bindings
import Crawl.LevelMap

explore :: LevelMap -> Coord -> T.Text
explore level loc = T.singleton $ case aStar adj cost bound isUnmapped loc of
  Just (loc' : _) -> moveTo loc loc'
  _ -> '.'
  where adj (Coord x y) = S.fromList [ target
                          | dx <- [-1,0,1], dy <- [-1,0,1], not (dx == 0 && dy == 0),
                            let target = Coord (x+dx) (y+dy),
                            maybe True isPassable (H.lookup target level) ]
        cost _ target = movementCost (level H.! target)
        bound _ = 0
        isUnmapped target = not $ target `H.member` level

moveTo :: Coord -> Coord -> Char
moveTo (Coord sx sy) (Coord tx ty) = case (tx - sx, ty - sy) of
  (-1,  0) -> 'h'
  ( 0,  1) -> 'j'
  ( 0, -1) -> 'k'
  ( 1,  0) -> 'l'
  (-1, -1) -> 'y'
  ( 1, -1) -> 'u'
  (-1,  1) -> 'b'
  ( 1,  1) -> 'n'
  _        -> error "tried to make illegal move"

isPassable :: Feature -> Bool
isPassable feat | DNGN_MANGROVE <= feat && feat <= DNGN_DEEP_WATER = False
isPassable _ = True

movementCost :: Feature -> Int
movementCost _ = 1
