{-# LANGUAGE OverloadedStrings #-}

module Crawl.Branch (
  DungeonLevel(..),
  parseBranch,
  branchDepth
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Hashable as H
import qualified Data.Text as T

import Crawl.Bindings (Branch(..))

data DungeonLevel = DungeonLevel Branch Int deriving (Eq)
instance H.Hashable DungeonLevel where
  hashWithSalt salt (DungeonLevel b d) = salt + 27 * fromEnum b + d

parseBranch :: T.Text -> Branch
parseBranch b = fromMaybe (error $ "unknown branch " ++ T.unpack b) $
                lookup b branchNames

-- TODO: autogenerate this from branch-data.h?
branchNames :: [(T.Text, Branch)]
branchNames = [
  ("Dungeon", BRANCH_DUNGEON),
  ("Temple", BRANCH_TEMPLE),
  ("Orcish Mines", BRANCH_ORC),
  ("Elven Halls", BRANCH_ELF),
  ("Lair", BRANCH_LAIR),
  ("Swamp", BRANCH_SWAMP),
  ("Shoals", BRANCH_SHOALS),
  ("Snake Pit", BRANCH_SNAKE),
  ("Spider Nest", BRANCH_SPIDER),
  ("Slime Pits", BRANCH_SLIME),
  ("Vaults", BRANCH_VAULTS),
  ("Crypt", BRANCH_CRYPT),
  ("Tomb", BRANCH_TOMB),
  ("Hell", BRANCH_VESTIBULE),
  -- skip hell branches for now
  ("Zot", BRANCH_ZOT),
  ("Abyss", BRANCH_ABYSS),
  -- skip pan/zig too

  -- Portal vaults
  ("a Labyrinth", BRANCH_LABYRINTH),
  ("a Bazaar", BRANCH_BAZAAR),
  ("a Trove", BRANCH_TROVE),
  ("a Sewer", BRANCH_SEWER),
  ("an Ossuary", BRANCH_OSSUARY),
  ("a Bailey", BRANCH_BAILEY),
  ("an Ice Cave", BRANCH_ICE_CAVE),
  ("a Volcano", BRANCH_VOLCANO),
  ("a Wizlab", BRANCH_WIZLAB),

  ("Depths", BRANCH_DEPTHS)
  ]

branchDepth :: Branch -> Int
branchDepth BRANCH_DUNGEON = 15
branchDepth BRANCH_TEMPLE = 1
branchDepth BRANCH_ORC = 3      -- not really but...
branchDepth BRANCH_ELF = 3
branchDepth BRANCH_LAIR = 8
branchDepth BRANCH_SWAMP = 4
branchDepth BRANCH_SHOALS = 4
branchDepth BRANCH_SNAKE = 4
branchDepth BRANCH_SPIDER = 4
branchDepth BRANCH_SLIME = 6
branchDepth BRANCH_VAULTS = 5
branchDepth BRANCH_CRYPT = 3
branchDepth BRANCH_TOMB = 3
branchDepth BRANCH_ZOT = 5
branchDepth BRANCH_ABYSS = 5
branchDepth BRANCH_DEPTHS = 5
branchDepth _ = 1               -- Portal vaults and bad places
