module Crawl.Item where

import Crawl.Bindings

-- Could move more type-dependent fields in here
-- (weapon/armour enchantments, wand charges, ...)
-- Could also use custom Unknown | Known a type instead of Maybe.
data ItemData = ItemWeapon WeaponType
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
              | ItemCorpse CorpseType Bool            -- rotting?
              | ItemGold
              | ItemRod (Maybe RodType)
              | ItemJunk
                -- XXX pseudo-item for anything we don't want
                -- and don't know how to parse yet
              deriving (Eq, Show)

data CurseStatus = Uncursed | Cursed

data Item = Item ItemData Int Color (Maybe CurseStatus)



itemData :: Item -> ItemData
itemData (Item d _ _ _) = d

itemCount :: Item -> Int
itemCount (Item _ count _ _) = count

itemColor :: Item -> Color
itemColor (Item _ _ color _) = color

knownCursed :: Item -> Bool
knownCursed (Item _ _ _ (Just Cursed)) = True
knownCursed _ = False

isVampiric :: Item -> Bool
isVampiric _item = False         -- XXX FIXME
