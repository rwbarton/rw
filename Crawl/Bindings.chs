module Crawl.Bindings where

#include "enums.h"

{# enum dungeon_feature_type as Feature {} deriving (Eq, Ord) #}

{# enum mouse_mode as MouseMode {} deriving (Eq, Show) #}

{# enum object_class_type as ItemBaseType {} deriving (Show) #}
{# enum weapon_type as WeaponType {} deriving (Eq, Ord) #}
{# enum missile_type as MissileType {} deriving (Eq, Ord) #}
{# enum armour_type as ArmourType {} deriving (Eq, Ord) #}
{# enum wand_type as WandType {} deriving (Eq, Ord) #}
{# enum food_type as FoodType {} deriving (Eq, Ord) #}
{# enum scroll_type as ScrollType {} deriving (Eq, Ord) #}
{# enum jewellery_type as JewelleryType {} deriving (Eq, Ord) #}
{# enum potion_type as PotionType {} deriving (Eq, Ord) #}
{# enum book_type as BookType {} deriving (Eq, Ord) #}
{# enum stave_type as StaffType {} deriving (Eq, Ord) #}
{# enum orb_type as OrbType {} deriving (Eq, Ord) #}
{# enum misc_item_type as MiscellanyType {} deriving (Eq, Ord) #}
{# enum corpse_type as CorpseType {} deriving (Eq, Ord) #}
{# enum rod_type as RodType {} deriving (Eq, Ord) #}
