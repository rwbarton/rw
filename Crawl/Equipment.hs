module Crawl.Equipment where

import Data.List (maximumBy, nub)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

import qualified Data.Map as M

import Crawl.Bindings
import Crawl.Inventory
import Crawl.Move
import Crawl.Status

upgradeEquipment :: Inventory -> Equipment -> Player -> Maybe Move -> Maybe Move
upgradeEquipment inv equip player morsel =
  (if isBerserk player then fmap (const Rest) else id) $
  case [ (equipSlot, invSlot, oldInvSlot)
       | let inv' = M.filter ((/= 8) . itemColour) inv, -- ignore unwearable items
         let availableSlots = nub $ mapMaybe (equipmentSlot . itemType) (M.elems inv'),
         equipSlot <- availableSlots,
         let oldInvSlot = M.lookup equipSlot equip,
         maybe True canUnwield $ (`M.lookup` inv) =<< oldInvSlot,
         not (equipSlot == EQ_BODY_ARMOUR && maybe False cursed ((`M.lookup` inv) =<< M.lookup EQ_CLOAK equip)),
         let invSlot = maximumBy (comparing score) $ map fst $
                       filter ((== Just equipSlot) . equipmentSlot . itemType . snd) $
                       M.toList inv',
         score invSlot > maybe 0 score oldInvSlot ] of
    (EQ_WEAPON, invSlot, _) : _
      | isVampiric (inv M.! invSlot) && hungerLevel player < HS_FULL
        -> morsel      -- Eat food if possible, otherwise go find some
      | otherwise -> Just (Wield invSlot)
    (_, _, Just oldInvSlot) : _ -> Just (TakeOff oldInvSlot)
    (_, invSlot, Nothing) : _ -> Just (Wear invSlot)
    [] -> Nothing
    where score slot = itemScore (inv M.! slot)

isEquipmentUpgrade :: Inventory -> ItemType -> Bool
isEquipmentUpgrade inv item = case equipmentSlot item of
  Just equipSlot -> itemTypeScore item > maximum (0 : map itemScore [ i | i <- M.elems inv, equipmentSlot (itemType i) == Just equipSlot ])
  Nothing -> False

canUnwield :: Item -> Bool
canUnwield item = not (cursed item) && not (isVampiric item)

-- This gets called after upgradeEquipment,
-- so anything that goes in a slot we have filled must be junk
-- (unless it's vampiric and we can't eat enough yet)
dropJunkEquipment :: Inventory -> Equipment -> Maybe Move
dropJunkEquipment inv equip =
  case [ invSlot
       | (invSlot, item) <- M.toList inv,
         not $ isVampiric item,
         Just equipSlot <- return (equipmentSlot $ itemType item),
         Just otherSlot <- return (M.lookup equipSlot equip),
         not $ cursed $ inv M.! otherSlot,
         not $ (equipSlot == EQ_BODY_ARMOUR && maybe False cursed ((`M.lookup` inv) =<< M.lookup EQ_CLOAK equip)),
         invSlot /= otherSlot ] of
    invSlot : _ -> Just (Drop invSlot)
    [] -> Nothing


equipmentSlot :: ItemType -> Maybe EquipmentSlot
equipmentSlot (ItemWeapon _) = Just EQ_WEAPON
equipmentSlot (ItemArmour ARM_CLOAK) = Just EQ_CLOAK
equipmentSlot (ItemArmour ARM_CAP) = Just EQ_HELMET
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

itemScore :: Item -> Int
itemScore item = itemTypeScore (itemType item) + if isVampiric item then 100 else 0

itemTypeScore :: ItemType -> Int
itemTypeScore (ItemWeapon w) = baseDamage w
itemTypeScore (ItemArmour a) = 2 * baseAC a - baseEVP a
itemTypeScore _ = 1

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
