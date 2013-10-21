module Crawl.Explore (
  kill, explore, loot, descend
  ) where

import Data.Graph.AStar (aStar)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Set as S

import Crawl.Bindings
import Crawl.LevelInfo
import Crawl.Move

data V = START | C { unC :: !Coord } deriving (Eq, Ord)

pathfind :: HS.HashSet Coord -> LevelInfo -> Coord -> Maybe [Coord]
pathfind goals info loc@(Coord lx ly) = fmap (tail . reverse . map unC) $ aStar adj cost bound isGoal START
  where adj START = S.fromList $ map C $ HS.toList goals
        adj (C (Coord x y)) = S.fromList [ C target
                                         | dx <- [-1,0,1], dy <- [-1,0,1], not (dx == 0 && dy == 0),
                                           let target = Coord (x+dx) (y+dy),
                                           maybe False isPassable (H.lookup target level) ]
        cost _ START = error "pathfind: START unreachable"
        cost _ (C target) = maybe 0 movementCost (H.lookup target level)
                            + maybe 0 (plantPenalty . _monsterType) (H.lookup target $ _levelMonsters info)
        bound START = 0
        bound (C (Coord x y)) = max (abs (x-lx)) (abs (y-ly))
        isGoal = (== C loc)
        level = _levelMap info

plantPenalty :: MonsterType -> Int
plantPenalty MONS_FUNGUS = 5
plantPenalty MONS_PLANT = 20
plantPenalty MONS_BUSH = 80
plantPenalty _ = 0

kill :: LevelInfo -> Coord -> Maybe Move
kill info loc = case pathfind (HS.fromList $ H.keys $ H.filter isRealMonster (_levelMonsters info)) info loc of
  Just [loc'] -> Just (attackTo loc loc')
  Just (loc' : _) -> Just (moveTo loc loc')
  _ -> Nothing
  where isRealMonster ty = case _monsterType ty of
          MONS_FUNGUS -> False
          MONS_PLANT -> False
          MONS_BUSH -> False
          _ -> True

explore :: LevelInfo -> Coord -> Maybe Move
explore info loc = case pathfind (_levelFringe info) info loc of
  Just (loc' : _) -> Just (moveTo loc loc')
  _ -> Nothing

loot :: LevelInfo -> Coord -> Maybe Move
loot info loc = case pathfind (HS.filter (not . isTeleTrap . (_levelMap info H.!)) $ _levelLoot info) info loc of
  Just (loc' : _) -> Just (moveTo loc loc')
  _ -> Nothing
  where isTeleTrap = (== DNGN_TRAP_MAGICAL) -- Not really, but the best we can do for now

descend :: LevelInfo -> Coord -> Maybe Move
descend info loc = case pathfind (HS.fromList $ H.keys $ H.filter isDownStair (_levelMap info)) info loc of
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

attackTo :: Coord -> Coord -> Move
attackTo (Coord sx sy) (Coord tx ty) = Attack (tx - sx) (ty - sy)

isPassable :: Feature -> Bool
isPassable feat | DNGN_MANGROVE <= feat && feat <= DNGN_DEEP_WATER = False
isPassable _ = True

movementCost :: Feature -> Int
movementCost DNGN_TRAP_MAGICAL = 10000
movementCost DNGN_RUNED_DOOR = 10000
movementCost DNGN_CLOSED_DOOR = 2
movementCost DNGN_SHALLOW_WATER = 2
movementCost _ = 1