{-# LANGUAGE OverloadedStrings #-}

module Crawl.ParseItem where

import Data.List (find)
import Data.Maybe (fromMaybe)

import qualified Data.Text as T

import Crawl.Bindings
import Crawl.Item

parseItem :: T.Text -> Item
parseItem itemName = Item (fromMaybe ItemJunk (parseItemData itemName)) 1 7 Nothing
                     -- XXX extract the real color, cursed status

parseItemData :: T.Text -> Maybe ItemData
parseItemData itemName = fmap snd $ find ((`T.isInfixOf` itemName) . (" " `T.append`) . fst) itemTypeNames

itemTypeNames :: [(T.Text, ItemData)]
itemTypeNames = [
  -- weapons we care about
  ("hand axe", ItemWeapon WPN_HAND_AXE),
  ("war axe", ItemWeapon WPN_WAR_AXE),
  ("broad axe", ItemWeapon WPN_BROAD_AXE),
  ("battleaxe", ItemWeapon WPN_BATTLEAXE),
  ("executioner's axe", ItemWeapon WPN_EXECUTIONERS_AXE),

  -- armours we care about
  ("ring mail", ItemArmour ARM_RING_MAIL),
  ("scale mail", ItemArmour ARM_SCALE_MAIL),
  ("chain mail", ItemArmour ARM_CHAIN_MAIL),
  ("plate armour", ItemArmour ARM_PLATE_ARMOUR),
  ("cloak", ItemArmour ARM_CLOAK),
  ("cap", ItemArmour ARM_CAP),
  ("helmet", ItemArmour ARM_HELMET),
  ("gloves", ItemArmour ARM_GLOVES),
  ("gauntlets", ItemArmour ARM_GLOVES),
  ("boots", ItemArmour ARM_BOOTS),
  ("crystal plate armour", ItemArmour ARM_CRYSTAL_PLATE_ARMOUR),

  -- add the hides too eventually
  ("fire dragon armour", ItemArmour ARM_FIRE_DRAGON_ARMOUR),
  ("ice dragon armour", ItemArmour ARM_ICE_DRAGON_ARMOUR),
  ("steam dragon armour", ItemArmour ARM_STEAM_DRAGON_ARMOUR),
  ("mottled dragon armour", ItemArmour ARM_MOTTLED_DRAGON_ARMOUR),
  ("storm dragon armour", ItemArmour ARM_STORM_DRAGON_ARMOUR),
  ("gold dragon armour", ItemArmour ARM_GOLD_DRAGON_ARMOUR),
  ("swamp dragon armour", ItemArmour ARM_SWAMP_DRAGON_ARMOUR),
  ("pearl dragon armour", ItemArmour ARM_PEARL_DRAGON_ARMOUR),

  -- food we care about
  ("meat ration", ItemFood FOOD_MEAT_RATION),
  ("bread ration", ItemFood FOOD_BREAD_RATION),
  ("royal jell", ItemFood FOOD_ROYAL_JELLY),
  ("fruit", ItemFood FOOD_FRUIT),
  ("pizza", ItemFood FOOD_PIZZA),
  ("beef jerky", ItemFood FOOD_BEEF_JERKY),

  ("chunk", ItemFood FOOD_CHUNK),

  -- potions
  ("potion", ItemPotion Nothing),

  -- scrolls
  ("scroll", ItemScroll Nothing),

  -- corpses
  ("rotting", ItemCorpse (error "CorpseType") True),
  ("corpse", ItemCorpse (error "CorpseType") False),

  -- books
  ("book", ItemBook Nothing),   -- XXX technically ItemBook includes manuals also

  -- others
  ("gold piece", ItemGold)
  ]
