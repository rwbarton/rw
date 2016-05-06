{-# LANGUAGE OverloadedStrings #-}

module Crawl.Explore (
  kill, explore, loot, findTrogAltar, findOtherAltar, enterBranches, descend
  ) where

import Data.List (isPrefixOf)
import Data.Graph.AStar (aStar)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Set as S
import qualified Data.Text as T

import Crawl.Bindings
import Crawl.Branch
import Crawl.FloorItems
import Crawl.Inventory
import Crawl.LevelInfo
import Crawl.Move
import Crawl.Status

data V = START | C { unC :: !Coord } deriving (Eq, Ord)

pathfind :: HS.HashSet Coord -> LevelInfo -> Coord -> Bool -> Maybe [Coord]
pathfind goals info loc@(Coord lx ly) desperate = fmap (tail . reverse . map unC) $ aStar adj cost bound isGoal START
  where adj START = S.fromList $ map C $ HS.toList goals
        adj (C (Coord x y)) = S.fromList [ C target
                                         | dx <- [-1,0,1], dy <- [-1,0,1], not (dx == 0 && dy == 0),
                                           let target = Coord (x+dx) (y+dy),
                                           maybe False isPassable (H.lookup target level),
                                           inCloud || maybe True isCloudSafe (H.lookup target (_levelClouds info)),
                                           desperate || maybe 0 movementCost (H.lookup target level) < 10000 ]
        cost _ START = error "pathfind: START unreachable"
        cost _ (C target) = maybe 0 movementCost (H.lookup target level)
                            + maybe 0 (plantPenalty . _monsterType) (H.lookup target $ _levelMonsters info)
        bound START = 0
        bound (C (Coord x y)) = max (abs (x-lx)) (abs (y-ly))
        isGoal = (== C loc)
        inCloud = not $ maybe True isCloudSafe (H.lookup loc (_levelClouds info))
        level = _levelMap info

plantPenalty :: MonsterType -> Int
plantPenalty MONS_FUNGUS = 5
plantPenalty MONS_PLANT = 20
plantPenalty MONS_BUSH = 80
plantPenalty _ = 0

kill :: LevelInfo -> Coord -> Player -> Maybe Move
kill info loc _player = case pathfind (HS.fromList $ H.keys $ H.filter isFoe (_levelMonsters info)) info loc False of
  Just [loc'] -> Just (attackTo loc loc')
  Just (loc' : _) -> Just (moveTo loc loc')
  _ -> Nothing
  where isFoe ty = _monsterAttitude ty == ATT_HOSTILE && isRealMonster ty
        isRealMonster ty = case _monsterType ty of
          MONS_FUNGUS -> False
          MONS_PLANT -> False
          MONS_BUSH -> False
          MONS_OKLOB_PLANT -> False -- Try to avoid fighting these
          MONS_BUTTERFLY -> False
          _ -> True

explore :: LevelInfo -> Coord -> Maybe Move
explore info loc = case pathfind (_levelFringe info) info loc False of
  Just (loc' : _) -> Just (moveTo loc loc')
  _ -> Nothing

loot :: LevelInfo -> Coord -> FloorItems -> Inventory -> Maybe Move
loot info loc items inv = case pathfind (HS.fromList $ H.keys $ H.filter (possiblyAny (wantItem False inv) . snd) items) info loc False of
  Just [] -> Just LookHere
  Just locs@(loc' : _)
    | all (not . isTeleTrap . (_levelMap info H.!)) locs -> Just (moveTo loc loc')
  _ -> Nothing
  where isTeleTrap = (== DNGN_TRAP_TELEPORT)

findTrogAltar :: LevelInfo -> Coord -> Maybe Move
findTrogAltar info loc = case pathfind (HS.fromList $ H.keys $ H.filter isTrogAltar (_levelMap info)) info loc False of
  Just [] -> Just Worship
  Just (loc' : _) -> Just (moveTo loc loc')
  _ -> Nothing
  where isTrogAltar DNGN_ALTAR_TROG = True
        isTrogAltar _ = False

findOtherAltar :: LevelInfo -> Coord -> [T.Text] -> Maybe Move
findOtherAltar info loc _fs = case pathfind (HS.fromList $ H.keys $ H.filter isOtherAltar (_levelMap info)) info loc False of
  Just [] -> Just Worship
  Just (loc' : _) -> Just (moveTo loc loc')
  _ -> Nothing
  where -- isOtherAltar DNGN_ALTAR_CHEIBRIADOS = True
        -- isOtherAltar DNGN_ALTAR_GOZAG = True
        --- isOtherAltar DNGN_ALTAR_QAZLAL = True
        --- isOtherAltar DNGN_ALTAR_DITHMENOS = True
        --- isOtherAltar DNGN_ALTAR_OKAWARU = True
        isOtherAltar DNGN_ALTAR_MAKHLEB = True
        --- isOtherAltar DNGN_ALTAR_SHINING_ONE = True
        --- isOtherAltar DNGN_ALTAR_ZIN = True

        -- isOtherAltar DNGN_ALTAR_ASHENZARI = True
        -- isOtherAltar DNGN_ALTAR_SIF_MUNA = True
        -- isOtherAltar DNGN_ALTAR_VEHUMET = "Vehumet" `notElem` fs
        -- isOtherAltar DNGN_ALTAR_KIKUBAAQUDGHA = True
        --- isOtherAltar DNGN_ALTAR_NEMELEX_XOBEH = null fs -- "Nemelex Xobeh" `notElem` fs
        --- isOtherAltar DNGN_ALTAR_MAKHLEB = null fs -- "Makhleb" `notElem` fs
        --- isOtherAltar DNGN_ALTAR_OKAWARU = null fs -- "Okawaru" `notElem` fs
        -- isOtherAltar DNGN_ALTAR_RU = True

        isOtherAltar _ = False

enterBranches :: LevelInfo -> Coord -> (DungeonLevel -> Bool) -> Maybe Move
enterBranches info loc beenTo = case pathfind (HS.fromList $ H.keys $ H.filter isBranchEntrance (_levelMap info)) info loc True of
  Just [] -> Just (stairDirection (_levelMap info H.! loc))
  Just (loc' : _) -> Just (moveTo loc loc')
  _ -> Nothing
  where isBranchEntrance DNGN_ENTER_BAZAAR = True
        isBranchEntrance DNGN_ENTER_SEWER = True
        isBranchEntrance DNGN_ENTER_OSSUARY = True
        isBranchEntrance DNGN_ENTER_BAILEY = True
        isBranchEntrance DNGN_ENTER_ICE_CAVE = True
        isBranchEntrance DNGN_ENTER_VOLCANO = True
        isBranchEntrance DNGN_ENTER_WIZLAB = True
        -- isBranchEntrance DNGN_ENTER_LABYRINTH = True

        isBranchEntrance DNGN_ENTER_PANDEMONIUM = True
        isBranchEntrance DNGN_TRANSIT_PANDEMONIUM = True
        -- isBranchEntrance DNGN_ENTER_HELL = not (beenToBranch BRANCH_VESTIBULE)
        -- isBranchEntrance DNGN_EXIT_HELL = True

        -- isBranchEntrance DNGN_ENTER_TEMPLE = not $ beenToBranch BRANCH_TEMPLE

        isBranchEntrance DNGN_STONE_STAIRS_DOWN_I = diving BRANCH_SNAKE || diving BRANCH_SPIDER || diving BRANCH_SWAMP || diving BRANCH_SHOALS
        isBranchEntrance DNGN_STONE_STAIRS_DOWN_II = diving BRANCH_SNAKE || diving BRANCH_SPIDER || diving BRANCH_SWAMP || diving BRANCH_SHOALS
        isBranchEntrance DNGN_STONE_STAIRS_DOWN_III = diving BRANCH_SNAKE || diving BRANCH_SPIDER || diving BRANCH_SWAMP || diving BRANCH_SHOALS
        isBranchEntrance DNGN_ESCAPE_HATCH_DOWN = diving BRANCH_SNAKE || diving BRANCH_SPIDER || diving BRANCH_SWAMP || diving BRANCH_SHOALS

        isBranchEntrance DNGN_ENTER_LAIR = not $ beenToBranch BRANCH_LAIR

        isBranchEntrance DNGN_ENTER_SNAKE = beenTo (DungeonLevel BRANCH_LAIR 8)
        isBranchEntrance DNGN_ENTER_SWAMP = beenTo (DungeonLevel BRANCH_LAIR 8)
        -- isBranchEntrance DNGN_ENTER_SHOALS = beenTo (DungeonLevel BRANCH_LAIR 8) -- shoals is a bad place imo
        isBranchEntrance DNGN_ENTER_SPIDER = beenTo (DungeonLevel BRANCH_LAIR 8)

{-
        isBranchEntrance DNGN_ENTER_SLIME = not $ beenToBranch BRANCH_SLIME
-}

        isBranchEntrance DNGN_ENTER_ORC = beenToBranch BRANCH_LAIR && (not $ beenToBranch BRANCH_ORC)
{-
beenToBranch BRANCH_LAIR && beenTo (DungeonLevel BRANCH_DUNGEON 15) &&
                                          (not $ beenToBranch BRANCH_ORC)
-}
        -- isBranchEntrance DNGN_ENTER_ELF = not $ beenToBranch BRANCH_ELF

        isBranchEntrance DNGN_ENTER_DEPTHS = beenToBranch BRANCH_ORC && (not $ beenToBranch BRANCH_DEPTHS)

        -- leave these places immediately
        isBranchEntrance DNGN_EXIT_ELF = True
        -- isBranchEntrance DNGN_EXIT_DEPTHS = True
{-
        isBranchEntrance DNGN_EXIT_SNAKE = True
        isBranchEntrance DNGN_EXIT_SWAMP = True
        isBranchEntrance DNGN_EXIT_SHOALS = True
        isBranchEntrance DNGN_EXIT_SPIDER = True
-}
        isBranchEntrance DNGN_EXIT_SLIME = True
        isBranchEntrance DNGN_EXIT_ICE_CAVE = True
        isBranchEntrance DNGN_EXIT_VOLCANO = True
        isBranchEntrance DNGN_EXIT_WIZLAB = True
        isBranchEntrance DNGN_EXIT_LABYRINTH = True

        isBranchEntrance DNGN_EXIT_ABYSS = True

        isBranchEntrance _ = False

        beenToBranch br = beenTo (DungeonLevel br 1)
        diving br = beenToBranch br && not (beenTo (DungeonLevel br (branchDepth br)))

descend :: LevelInfo -> Coord -> DungeonLevel -> (DungeonLevel -> Bool) -> Maybe Move
descend info loc dl beenTo = case pathfind (HS.fromList $ H.keys $ H.filter isDownStair (_levelMap info)) info loc True of
  Just [] -> Just (stairDirection (_levelMap info H.! loc))
  Just (loc' : _) -> Just (moveTo loc loc')
  _ -> Nothing
  where isDownStair DNGN_STONE_STAIRS_DOWN_I = not exitingBranch
        isDownStair DNGN_STONE_STAIRS_DOWN_II = not exitingBranch
        isDownStair DNGN_STONE_STAIRS_DOWN_III = not exitingBranch
        isDownStair DNGN_ESCAPE_HATCH_DOWN = not exitingBranch

        isDownStair DNGN_STONE_STAIRS_UP_I = exitingBranch
        isDownStair DNGN_STONE_STAIRS_UP_II = exitingBranch
        isDownStair DNGN_STONE_STAIRS_UP_III = exitingBranch
        isDownStair DNGN_ESCAPE_HATCH_UP = exitingBranch
        isDownStair DNGN_EXIT_LAIR = exitingBranch
        isDownStair DNGN_EXIT_ORC = exitingBranch

        isDownStair DNGN_EXIT_TEMPLE = True

        isDownStair DNGN_EXIT_ZIGGURAT = True
        isDownStair DNGN_EXIT_BAZAAR = True
        isDownStair DNGN_EXIT_TROVE = True
        isDownStair DNGN_EXIT_SEWER = True
        isDownStair DNGN_EXIT_OSSUARY = True
        isDownStair DNGN_EXIT_BAILEY = True
        isDownStair DNGN_EXIT_ICE_CAVE = True
        isDownStair DNGN_EXIT_VOLCANO = True
        isDownStair DNGN_EXIT_WIZLAB = True
        isDownStair DNGN_EXIT_LABYRINTH = True
        isDownStair DNGN_EXIT_ABYSS = True
        isDownStair _ = False

        DungeonLevel br d = dl
        exitingBranch = all beenTo [ DungeonLevel br d'
                                   | d' <- [d+1..branchDepth br] ] && not (br == BRANCH_DUNGEON && beenTo (DungeonLevel BRANCH_ORC 1))

stairDirection :: Feature -> Move
stairDirection DNGN_STONE_STAIRS_UP_I = GoUp
stairDirection DNGN_STONE_STAIRS_UP_II = GoUp
stairDirection DNGN_STONE_STAIRS_UP_III = GoUp
stairDirection DNGN_ESCAPE_HATCH_UP = GoUp
stairDirection DNGN_EXIT_LABYRINTH = GoUp
stairDirection DNGN_EXIT_HELL = GoUp
stairDirection feat | DNGN_EXIT_ORC <= feat && feat <= DNGN_EXIT_DEPTHS = GoUp
stairDirection _ = GoDown

moveTo :: Coord -> Coord -> Move
moveTo (Coord sx sy) (Coord tx ty) = Go (tx - sx) (ty - sy)

attackTo :: Coord -> Coord -> Move
attackTo (Coord sx sy) (Coord tx ty) = Attack (tx - sx) (ty - sy)

isPassable :: Feature -> Bool
isPassable feat | DNGN_TREE <= feat && feat <= DNGN_DEEP_WATER = False
isPassable _ = True

movementCost :: Feature -> Int
movementCost DNGN_TRAP_TELEPORT = 10000
movementCost DNGN_TRAP_ZOT = 10000
movementCost DNGN_RUNED_DOOR = 10000
movementCost DNGN_CLOSED_DOOR = 2
movementCost DNGN_SHALLOW_WATER = 2
movementCost _ = 1

-- XXX Memoize this
isCloudSafe :: Tile -> Bool
isCloudSafe c = not $ any (`isBaseNameOf` show c)
  ["TILE_CLOUD_FIRE", "TILE_CLOUD_COLD",
   -- poison, meph, miasma okay for gr
   "TILE_CLOUD_MUTAGENIC", "TILE_CLOUD_CHAOS",
   "TILE_CLOUD_RAGING_WINDS",
   -- petrification okay for gr
   "TILE_CLOUD_FOREST_FIRE", "TILE_CLOUD_GHOSTLY_FLAME",
   "TILE_CLOUD_ACID", "TILE_CLOUD_STORM", "TILE_CLOUD_NEG"]
  where name `isBaseNameOf` s = name == s || (name ++ "_") `isPrefixOf` s
