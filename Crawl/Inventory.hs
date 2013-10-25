{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# OPTIONS_GHC -O #-}  -- Weird bug in GHC 7.4.2 with -O2 (#7165)

module Crawl.Inventory (
  Item, ItemType(..), itemType, cursed,
  Inventory, InventorySlot, slotLetter,
  inventory,
  Equipment, equipment
  ) where

import Data.Char (chr, ord)
import Control.Monad.Trans.State (execState)
import Data.Foldable (forM_)

import Control.Lens (makeLenses, iforMOf_, itraversed, (^?), ix, (.=), at, (^.))
import Control.Lens.Aeson (key, _Object, _Integer, _String)
import Data.Bits.Lens (bitAt)
import Numeric.Lens (integral)
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Reactive.Banana as R

import Crawl.Bindings


data Item = Item {
  _base_type :: !Int,
  _sub_type :: !Int,
  _plus :: !Int,
  _plus2 :: !Int,
  _quantity :: !Int,
  _flags :: !Int,
  _inscription :: !T.Text,
  _name :: !T.Text
  } deriving (Eq, Show)         -- Eq for 'non'

makeLenses ''Item

-- Could move more type-dependent fields in here
-- (weapon/armour enchantments, wand charges, ...)
-- Could also use custom Unknown | Known a type instead of Maybe.
data ItemType = ItemWeapon WeaponType
              | ItemMissile MissileType
              | ItemArmour ArmourType -- also has plus2 = sub-subtype
              | ItemWand (Maybe WandType)
              | ItemFood FoodType
              | ItemScroll (Maybe ScrollType)
              | ItemJewellery (Maybe JewelleryType) -- could split these
              | ItemPotion (Maybe PotionType)
              | ItemBook (Maybe BookType) -- includes manuals
              | ItemStaff (Maybe StaffType)
              | ItemOrb OrbType
              | ItemMiscellany (Maybe MiscellanyType) -- unided = deck
              | ItemCorpse CorpseType
              | ItemGold
              | ItemRod (Maybe RodType)
              deriving Eq

itemType :: Item -> ItemType
itemType Item { _base_type = bt, _sub_type = st } = case toEnum bt of
  OBJ_WEAPONS    -> ItemWeapon     $ toEnum st
  OBJ_MISSILES   -> ItemMissile    $ toEnum st
  OBJ_ARMOUR     -> ItemArmour     $ toEnum st
  OBJ_WANDS      -> ItemWand       $ maybeToEnum [NUM_WANDS] st
  OBJ_FOOD       -> ItemFood       $ toEnum st
  OBJ_SCROLLS    -> ItemScroll     $ maybeToEnum [NUM_SCROLLS] st
  OBJ_JEWELLERY  -> ItemJewellery  $ maybeToEnum [NUM_RINGS, NUM_JEWELLERY] st
  OBJ_POTIONS    -> ItemPotion     $ maybeToEnum [NUM_POTIONS] st
  OBJ_BOOKS      -> ItemBook       $ maybeToEnum [NUM_BOOKS] st
  OBJ_STAVES     -> ItemStaff      $ maybeToEnum [NUM_STAVES] st
  OBJ_ORBS       -> ItemOrb        $ toEnum st
  OBJ_MISCELLANY -> ItemMiscellany $ maybeToEnum [NUM_MISCELLANY] st
  OBJ_CORPSES    -> ItemCorpse     $ toEnum st
  OBJ_GOLD       -> ItemGold
  OBJ_RODS       -> ItemRod        $ maybeToEnum [NUM_RODS] st
  other          -> error $ "unexpected item base_type " ++ show other
  where maybeToEnum unknowns s =
          let t = toEnum s in if t `elem` unknowns then Nothing else Just t

cursed :: Item -> Bool
cursed item = item ^. flags.bitAt 8


newtype InventorySlot = InventorySlot Int deriving (Eq, Ord, Show)
type Inventory = M.Map InventorySlot Item

slotLetter :: InventorySlot -> Char
slotLetter (InventorySlot n)
  |  0 <= n && n < 26 = chr (ord 'a' + n)
  | 26 <= n && n < 52 = chr (ord 'A' + n - 26)
  | otherwise         = error $ "inventory slot " ++ show n ++ " out of range 0-51"

emptySlot :: Item
emptySlot = Item 100 0 0 0 0 0 "" "!bad item (cl:100,ty:1,pl:0,pl2:0,sp:0,qu:0)"

emptyInventory :: Inventory
emptyInventory = M.fromList [ (InventorySlot slot, emptySlot) | slot <- [0..51] ]

dropEmptySlots :: Inventory -> Inventory
dropEmptySlots = M.filter ((/= fromEnum OBJ_UNASSIGNED) . _base_type)

inventory :: R.Event t A.Value -> R.Behavior t Inventory
inventory input = fmap dropEmptySlots $ R.accumB emptyInventory $ fmap updateInventory input
  where updateInventory msg = execState $ iforMOf_ (key "inv"._Object.itraversed) msg updateSlot
        updateSlot slotName item = do
          updateIntField "base_type" base_type
          updateIntField "sub_type" sub_type
          updateIntField "plus" plus
          updateIntField "plus2" plus2
          updateIntField "quantity" quantity
          updateIntField "flags" flags
          updateTextField "inscription" inscription
          updateTextField "name" name
          where updateIntField s l = forM_ (item ^? key s._Integer.integral) $ \val -> ix slot.l .= val
                updateTextField s l = forM_ (item ^? key s._String) $ \val -> ix slot.l .= val
                slot = InventorySlot (read $ T.unpack slotName)


type Equipment = M.Map EquipmentSlot InventorySlot

equipment :: R.Event t A.Value -> R.Behavior t Equipment
equipment input = R.accumB M.empty $ fmap updateEquipment input
  where updateEquipment msg = execState $ iforMOf_ (key "equip"._Object.itraversed._Integer.integral) msg updateSlot
        updateSlot slotName invSlot = at (toEnum $ read $ T.unpack slotName) .= (if invSlot == -1 then Nothing else Just (InventorySlot invSlot))
