{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -O #-}  -- Weird bug in GHC 7.4.2 with -O2 (#7165)

module Crawl.Inventory (
  Inventory, InventorySlot, slotLetter,
  inventory,
  Equipment, equipment
  ) where

import Data.Char (chr, ord)
import Control.Monad.Trans.State (execState)
import Data.Foldable (forM_)

import Control.Lens (makeLenses, iforMOf_, itraversed, (^?), ix, (.=), at, (^.))
import Data.Aeson.Lens (key, _Object, _Integer, _String)
import Data.Bits.Lens (bitAt)
import Numeric.Lens (integral)
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Reactive.Banana as R

import Crawl.Bindings
import Crawl.Item


data InventoryItem = InventoryItem {
  _ii_base_type :: !Int,
  _ii_sub_type :: !Int,
  _ii_plus :: !Int,
  _ii_plus2 :: !Int,
  _ii_quantity :: !Int,
  _ii_flags :: !Int,
  _ii_inscription :: !T.Text,
  _ii_name :: !T.Text,
  _ii_col :: !Int
  } deriving (Eq, Show)         -- Eq for 'non'

makeLenses ''InventoryItem

inventoryItemData :: InventoryItem -> ItemData
inventoryItemData InventoryItem { _ii_base_type = bt, _ii_sub_type = st } = case toEnum bt of
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
  OBJ_CORPSES    -> error "impossible: corpse in inventory"
  OBJ_GOLD       -> ItemGold
  OBJ_RODS       -> ItemRod        $ maybeToEnum [NUM_RODS] st
  other          -> error $ "unexpected item base_type " ++ show other
  where maybeToEnum unknowns s =
          let t = toEnum s in if t `elem` unknowns then Nothing else Just t

inventoryItem :: InventoryItem -> Item
inventoryItem ii = Item (inventoryItemData ii) (ii ^. ii_quantity) (ii ^. ii_col) cursedStatus
  where cursedStatus = if not (ii ^. ii_flags.bitAt 0) -- ISFLAG_KNOW_CURSE
                       then Nothing
                       else Just $
                            if ii ^. ii_flags.bitAt 8 -- ISFLAG_CURSED
                            then Cursed
                            else Uncursed


newtype InventorySlot = InventorySlot Int deriving (Eq, Ord, Show)
type Inventory = M.Map InventorySlot Item
type RawInventory = M.Map InventorySlot InventoryItem

slotLetter :: InventorySlot -> Char
slotLetter (InventorySlot n)
  |  0 <= n && n < 26 = chr (ord 'a' + n)
  | 26 <= n && n < 52 = chr (ord 'A' + n - 26)
  | otherwise         = error $ "inventory slot " ++ show n ++ " out of range 0-51"

emptySlot :: InventoryItem
emptySlot = InventoryItem 100 0 0 0 0 0 "" "!bad item (cl:100,ty:1,pl:0,pl2:0,sp:0,qu:0)" 0

emptyInventory :: RawInventory
emptyInventory = M.fromList [ (InventorySlot slot, emptySlot) | slot <- [0..51] ]

dropEmptySlots :: RawInventory -> RawInventory
dropEmptySlots = M.filter ((/= fromEnum OBJ_UNASSIGNED) . _ii_base_type)

inventory :: R.Event t A.Value -> R.Behavior t Inventory
inventory input = fmap (fmap inventoryItem . dropEmptySlots) $ R.accumB emptyInventory $ fmap updateInventory input
  where updateInventory msg = execState $ iforMOf_ (key "inv"._Object.itraversed) msg updateSlot
        updateSlot slotName (item :: A.Value) = do
          updateIntField "base_type" ii_base_type
          updateIntField "sub_type" ii_sub_type
          updateIntField "plus" ii_plus
          updateIntField "plus2" ii_plus2
          updateIntField "quantity" ii_quantity
          updateIntField "flags" ii_flags
          updateTextField "inscription" ii_inscription
          updateTextField "name" ii_name
          updateIntField "col" ii_col
          where updateIntField s l = forM_ (item ^? key s._Integer.integral) $ \val -> ix slot.l .= val
                updateTextField s l = forM_ (item ^? key s._String) $ \val -> ix slot.l .= val
                slot = InventorySlot (read $ T.unpack slotName)


type Equipment = M.Map EquipmentSlot InventorySlot

equipment :: R.Event t A.Value -> R.Behavior t Equipment
equipment input = R.accumB M.empty $ fmap updateEquipment input
  where updateEquipment msg = execState $ iforMOf_ (key "equip"._Object.itraversed._Integer.integral) msg updateSlot
        updateSlot slotName invSlot = at (toEnum $ read $ T.unpack slotName) .= (if invSlot == -1 then Nothing else Just (InventorySlot invSlot))
