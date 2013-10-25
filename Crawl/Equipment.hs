module Crawl.Equipment where

import Data.List (maximumBy, nub)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

import qualified Data.Map as M

import Crawl.Bindings
import Crawl.Inventory
import Crawl.Move

upgradeEquipment :: Inventory -> Equipment -> Maybe Move
upgradeEquipment inv equip =
  case [ (equipSlot, invSlot, oldInvSlot)
       | let availableSlots = nub $ mapMaybe (equipmentSlot . itemType) (M.elems inv),
         equipSlot <- availableSlots,
         let oldInvSlot = M.lookup equipSlot equip,
         let invSlot = maximumBy (comparing score) $ map fst $
                       filter ((== Just equipSlot) . equipmentSlot . itemType . snd) $
                       M.toList inv,
         score invSlot > maybe 0 score oldInvSlot ] of
    (EQ_WEAPON, invSlot, _) : _ -> Just (Wield invSlot)
    (_, _, Just oldInvSlot) : _ -> Just (TakeOff oldInvSlot)
    (_, invSlot, Nothing) : _ -> Just (Wear invSlot)
    [] -> Nothing
    where score slot = itemScore (itemType $ inv M.! slot)

isEquipmentUpgrade :: Inventory -> ItemType -> Bool
isEquipmentUpgrade inv item = case equipmentSlot item of
  Just equipSlot -> itemScore item > maximum (0 : map itemScore [ i | i <- map itemType (M.elems inv), equipmentSlot i == Just equipSlot ])
  Nothing -> False



equipmentSlot :: ItemType -> Maybe EquipmentSlot
equipmentSlot (ItemWeapon _) = Just EQ_WEAPON
equipmentSlot (ItemArmour ARM_CLOAK) = Just EQ_CLOAK
equipmentSlot (ItemArmour ARM_CAP) = Just EQ_HELMET
equipmentSlot (ItemArmour ARM_WIZARD_HAT) = Just EQ_HELMET
equipmentSlot (ItemArmour ARM_HELMET) = Just EQ_HELMET
equipmentSlot (ItemArmour ARM_GLOVES) = Just EQ_GLOVES
equipmentSlot (ItemArmour ARM_BOOTS) = Just EQ_BOOTS
equipmentSlot (ItemArmour b)
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
              ARM_PEARL_DRAGON_HIDE, ARM_PEARL_DRAGON_ARMOUR] =
    Just EQ_BODY_ARMOUR
equipmentSlot _ = Nothing

itemScore :: ItemType -> Int
itemScore (ItemWeapon w) = baseDamage w
itemScore (ItemArmour a) = 2 * baseAC a - baseEVP a
itemScore _ = 1

-- Should automatically generate all of these from crawl...

baseDamage :: WeaponType -> Int
baseDamage WPN_HAND_AXE = 7
baseDamage WPN_WAR_AXE = 11
baseDamage WPN_BROAD_AXE = 13
baseDamage WPN_BATTLEAXE = 15
baseDamage WPN_EXECUTIONERS_AXE = 18

baseAC :: ArmourType -> Int
baseAC ARM_ANIMAL_SKIN = 2
baseAC ARM_RING_MAIL = 5
baseAC ARM_SCALE_MAIL = 6
baseAC ARM_CHAIN_MAIL = 7
baseAC ARM_PLATE_ARMOUR = 10

baseAC ARM_CLOAK = 1
baseAC ARM_CAP = 0
baseAC ARM_WIZARD_HAT = 0
baseAC ARM_HELMET = 1
baseAC ARM_GLOVES = 1
baseAC ARM_BOOTS = 1

baseEVP :: ArmourType -> Int
baseEVP ARM_ANIMAL_SKIN = 0
baseEVP ARM_RING_MAIL = 2
baseEVP ARM_SCALE_MAIL = 3
baseEVP ARM_CHAIN_MAIL = 4
baseEVP ARM_PLATE_ARMOUR = 6

baseEVP ARM_CLOAK = 0
baseEVP ARM_CAP = 0
baseEVP ARM_WIZARD_HAT = 0
baseEVP ARM_HELMET = 0
baseEVP ARM_GLOVES = 0
baseEVP ARM_BOOTS = 0