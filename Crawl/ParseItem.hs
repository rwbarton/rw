{-# LANGUAGE OverloadedStrings #-}

module Crawl.ParseItem where

import Control.Applicative ((<|>))
import Data.List (find)
import Data.Maybe (fromMaybe)

import Data.Attoparsec.Text
import qualified Data.Text as T

import Crawl.Bindings
import Crawl.ColoredText
import Crawl.Item

parseItem :: T.Text -> Item
parseItem itemName = Item (fromMaybe ItemJunk (parseItemData name)) 1 color Nothing
                     -- XXX extract the real cursed status
  where (color, name) = case parseColoredText itemName of
          [ct] -> ct
          [ct, (LIGHTGRAY, ".")] -> ct
          cts -> error $ "parseItem: item name " ++ show itemName ++ " did not parse as a single colored text: " ++ show cts

parseItemData :: T.Text -> Maybe ItemData
parseItemData itemName =
  parseWeapon itemName <|>
  (fmap snd $ find ((`T.isInfixOf` itemName) . (" " `T.append`) . fst) itemTypeNames)

parseWeapon :: T.Text -> Maybe ItemData
parseWeapon itemName =
  fmap (\(_, wt) -> ItemWeapon wt plus brand) $
  find ((`T.isInfixOf` itemName) . (" " `T.append`) . fst) weaponTypeNames
  where plus = maybeResult $ parse plusParser itemName
        plusParser = fmap (read . T.unpack . T.dropWhile (== '+')) $
                     (string "a " <|> string "the " <|> string "A " <|> string "The ") >>
                     takeWhile1 (\c -> c == '+' || c == '-' || '0' <= c && c <= '9')
        brand = (fmap snd $ find ((`T.isInfixOf` itemName) . (" " `T.append`) . fst) weaponBrandNames)
                 <|> -- if no brand in the name, it's normal iff we know the plus
                 fmap (const SPWPN_NORMAL) plus

weaponTypeNames :: [(T.Text, WeaponType)]
weaponTypeNames = [
  -- weapons we care about
  ("hand axe", WPN_HAND_AXE),
  ("war axe", WPN_WAR_AXE),
  ("broad axe", WPN_BROAD_AXE),
  ("battleaxe", WPN_BATTLEAXE),
  ("executioner's axe", WPN_EXECUTIONERS_AXE)
  ]

-- non-terse names
weaponBrandNames :: [(T.Text, WeaponBrand)]
weaponBrandNames = [
  ("of flaming", SPWPN_FLAMING),
  ("of freezing", SPWPN_FREEZING),
  ("of holy wrath", SPWPN_HOLY_WRATH),
  ("of electrocution", SPWPN_ELECTROCUTION),
  ("of venom", SPWPN_VENOM),
  ("of protection", SPWPN_PROTECTION),
  ("of draining", SPWPN_DRAINING),
  ("of speed", SPWPN_SPEED),
  ("of pain", SPWPN_PAIN),
  ("of distortion", SPWPN_DISTORTION),
  ("of reaping", SPWPN_REAPING),
  ("vampiric ", SPWPN_VAMPIRISM),
  ("of chopping", SPWPN_VORPAL), -- XXX axes are always chopping but add others
  ("antimagic ", SPWPN_ANTIMAGIC),
  ("of penetration", SPWPN_PENETRATION),
  ("of evasion", SPWPN_EVASION),
  ("of chaos", SPWPN_CHAOS)
  ]

itemTypeNames :: [(T.Text, ItemData)]
itemTypeNames = [
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
