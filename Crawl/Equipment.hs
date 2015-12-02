{-# LANGUAGE ViewPatterns #-}

module Crawl.Equipment where

import Data.List (maximumBy, nub)
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import Data.Ord (comparing)

import qualified Data.Map as M

import Crawl.Bindings
import Crawl.Inventory
import Crawl.Item
import Crawl.Move
import Crawl.Status

upgradeEquipment :: Inventory -> Equipment -> Player -> Maybe Move -> Maybe Move
upgradeEquipment inv equip player morsel =
  (if isBerserk player then fmap (const Rest) else id) $
  case [ (equipSlot, invSlot, oldInvSlot)
       | let inv' = M.filter ((/= DARKGRAY) . itemColor) inv, -- ignore unwearable items
         let availableSlots = nub $ mapMaybe equipmentSlot (M.elems inv'),
         equipSlot <- availableSlots,
         let oldInvSlot = M.lookup equipSlot equip,
         let invSlot = maximumBy (comparing score) $ map fst $
                       filter ((== Just equipSlot) . equipmentSlot . snd) $
                       M.toList inv',
         score invSlot > maybe 0 score oldInvSlot ] of
    (_, _, (>>= (`M.lookup` inv)) -> Just oldItem) : _
      | knownCursed oldItem -> uncurse
    (EQ_WEAPON, invSlot, _) : _
      | isVampiric (inv M.! invSlot) && hungerLevel player < HS_FULL
        -> morsel      -- Eat food if possible, otherwise go find some
      | otherwise -> Just (Wield invSlot)
    (_, _, Just oldInvSlot) : _ -> Just (TakeOff oldInvSlot)
    (_, invSlot, Nothing) : _ -> Just (Wear invSlot)
    [] -> Nothing
    where score slot = itemScore (inv M.! slot)
          uncurse = listToMaybe [ Read slot | (slot, itemData -> ItemScroll (Just SCR_REMOVE_CURSE)) <- M.toList inv ]

isEquipmentUpgrade :: Inventory -> Item -> Bool
isEquipmentUpgrade inv item = case equipmentSlot item of
  Just equipSlot -> itemScore item > maximum (0 : map itemScore [ i | i <- M.elems inv, equipmentSlot i == Just equipSlot ])
                    -- there is a tricky scenario we don't handle well:
                    -- if our current weapon is cursed and we've found one potentially better weapon,
                    -- we'll then evaluate that weapon optimistically which means we won't pick up
                    -- other weapons of similar nature. itemScore should really be an interval
  Nothing -> False

-- This gets called after upgradeEquipment,
-- so anything that goes in a slot we have filled must be junk
-- (unless it's vampiric and we can't eat enough yet)
dropJunkEquipment :: Inventory -> Equipment -> Maybe Move
dropJunkEquipment inv equip =
  case [ invSlot
       | (invSlot, item) <- M.toList inv,
         not $ isVampiric item,
         Just equipSlot <- return (equipmentSlot item),
         Just otherSlot <- return (M.lookup equipSlot equip),
         not $ knownCursed $ inv M.! otherSlot,
         not $ (equipSlot == EQ_BODY_ARMOUR && maybe False knownCursed ((`M.lookup` inv) =<< M.lookup EQ_CLOAK equip)),
         invSlot /= otherSlot ] of
    invSlot : _ -> Just (Drop invSlot)
    [] -> Nothing

enchantEquipment :: Inventory -> Equipment -> Maybe Move
enchantEquipment inv _equip =
  -- Presumably we always have something to enchant
  listToMaybe [ Read slot | (slot, itemData -> ItemScroll (Just s)) <- M.toList inv, s `elem` [SCR_ENCHANT_WEAPON, SCR_ENCHANT_ARMOUR] ]

equipmentSlot :: Item -> Maybe EquipmentSlot
equipmentSlot item = case itemData item of
  ItemWeapon {} -> Just EQ_WEAPON
  ItemArmour ARM_CLOAK -> Just EQ_CLOAK
  ItemArmour ARM_CAP -> Just EQ_HELMET
  ItemArmour ARM_HELMET -> Just EQ_HELMET
  ItemArmour ARM_GLOVES -> Just EQ_GLOVES
  ItemArmour ARM_BOOTS -> Just EQ_BOOTS
  ItemArmour b
    | b `elem` [ARM_ROBE, ARM_LEATHER_ARMOUR, ARM_RING_MAIL,
                ARM_SCALE_MAIL, ARM_CHAIN_MAIL, ARM_PLATE_ARMOUR,
                ARM_CRYSTAL_PLATE_ARMOUR, ARM_ANIMAL_SKIN,
                ARM_TROLL_HIDE, ARM_TROLL_LEATHER_ARMOUR,
                ARM_FIRE_DRAGON_HIDE, ARM_FIRE_DRAGON_ARMOUR,
                ARM_ICE_DRAGON_HIDE, ARM_ICE_DRAGON_ARMOUR,
                ARM_STEAM_DRAGON_HIDE, ARM_STEAM_DRAGON_ARMOUR,
                ARM_MOTTLED_DRAGON_HIDE, ARM_MOTTLED_DRAGON_ARMOUR,
                ARM_STORM_DRAGON_HIDE, ARM_STORM_DRAGON_ARMOUR,
                ARM_GOLD_DRAGON_HIDE, ARM_GOLD_DRAGON_ARMOUR,
                ARM_SWAMP_DRAGON_HIDE, ARM_SWAMP_DRAGON_ARMOUR,
                ARM_PEARL_DRAGON_HIDE, ARM_PEARL_DRAGON_ARMOUR]
      -> Just EQ_BODY_ARMOUR
  _ -> Nothing

itemScore :: Item -> Int
itemScore item = itemTypeScore (itemData item)

itemTypeScore :: ItemData -> Int
itemTypeScore (ItemWeapon w (fromMaybe 3 -> plus) (fromMaybe SPWPN_VAMPIRISM -> brand))
  = brandModify brand (150 * baseDamage w + 100 * plus)
  where brandModify SPWPN_NORMAL s = s
        brandModify SPWPN_FLAMING s = 5 * s `div` 4
        brandModify SPWPN_FREEZING s = 4 * s `div` 3 -- little extra bonus for Lair slowing
        brandModify SPWPN_HOLY_WRATH s = s + 50
        brandModify SPWPN_ELECTROCUTION s = s + 900
        brandModify SPWPN_VENOM s = s + 400
        brandModify SPWPN_PROTECTION s = s + 500
        brandModify SPWPN_DRAINING s = 9 * s `div` 8 + 200
        brandModify SPWPN_SPEED s = 7 * s `div` 4
        brandModify SPWPN_PAIN s = s
        brandModify SPWPN_DISTORTION s = 3 * s `div` 2 + 900
        brandModify SPWPN_VAMPIRISM s = 2 * s
        brandModify SPWPN_VORPAL s = 9 * s `div` 8
        brandModify SPWPN_ANTIMAGIC s = s + 200
        brandModify SPWPN_CHAOS s = s + 600      -- chaos is fun
        brandModify unknownBrand _ = error $ "unhandled brand in itemTypeScore: " ++ show unknownBrand
itemTypeScore (ItemArmour a) = 2 * baseAC a - baseEVP a
itemTypeScore _ = 1

-- Should automatically generate all of these from crawl...

baseDamage :: WeaponType -> Int
baseDamage WPN_HAND_AXE = 7
baseDamage WPN_WAR_AXE = 11
baseDamage WPN_BROAD_AXE = 13
-- baseDamage WPN_BATTLEAXE = 15
-- baseDamage WPN_EXECUTIONERS_AXE = 18
baseDamage _ = 0                -- in case we accidentally pick up some junk

baseAC :: ArmourType -> Int
baseAC ARM_ANIMAL_SKIN = 2
baseAC ARM_RING_MAIL = 5
baseAC ARM_SCALE_MAIL = 6
baseAC ARM_CHAIN_MAIL = 7
baseAC ARM_PLATE_ARMOUR = 10

baseAC ARM_CRYSTAL_PLATE_ARMOUR = 14
baseAC ARM_FIRE_DRAGON_ARMOUR = 8
baseAC ARM_ICE_DRAGON_ARMOUR = 9
baseAC ARM_STEAM_DRAGON_ARMOUR = 5
baseAC ARM_MOTTLED_DRAGON_ARMOUR = 6
baseAC ARM_STORM_DRAGON_ARMOUR = 10
baseAC ARM_GOLD_DRAGON_ARMOUR = 12
baseAC ARM_SWAMP_DRAGON_ARMOUR = 7
baseAC ARM_PEARL_DRAGON_ARMOUR = 10

baseAC ARM_CLOAK = 1
baseAC ARM_CAP = 0
baseAC ARM_HELMET = 1
baseAC ARM_GLOVES = 1
baseAC ARM_BOOTS = 1

baseAC _ = 0              -- in case we accidentally pick up some junk

baseEVP :: ArmourType -> Int
baseEVP ARM_ANIMAL_SKIN = 0
baseEVP ARM_RING_MAIL = 2
baseEVP ARM_SCALE_MAIL = 3
baseEVP ARM_CHAIN_MAIL = 4
baseEVP ARM_PLATE_ARMOUR = 6

baseEVP ARM_CRYSTAL_PLATE_ARMOUR = 8
baseEVP ARM_FIRE_DRAGON_ARMOUR = 3
baseEVP ARM_ICE_DRAGON_ARMOUR = 3
baseEVP ARM_STEAM_DRAGON_ARMOUR = 0
baseEVP ARM_MOTTLED_DRAGON_ARMOUR = 1
baseEVP ARM_STORM_DRAGON_ARMOUR = 5
baseEVP ARM_GOLD_DRAGON_ARMOUR = 9
baseEVP ARM_SWAMP_DRAGON_ARMOUR = 2
baseEVP ARM_PEARL_DRAGON_ARMOUR = 3

baseEVP ARM_CLOAK = 0
baseEVP ARM_CAP = 0
baseEVP ARM_HELMET = 0
baseEVP ARM_GLOVES = 0
baseEVP ARM_BOOTS = 0

baseEVP _ = 0             -- in case we accidentally pick up some junk
