{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Crawl.Status where

import Control.Applicative (liftA2)
import Control.Monad.Trans.State (execState)
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Foldable as F
import Control.Lens ((^?), (.=), to)
import Control.Lens.TH (makeLenses)
import Data.Aeson.Lens (_Integer, _String, _Array, key)
import Numeric.Lens (integral)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H
import qualified Reactive.Banana as R
import qualified Data.Text as T

import Crawl.Bindings (HungerLevel(..))
import Crawl.Branch (DungeonLevel(..), parseBranch)

data Player = Player {
  _species :: !T.Text,
  _god :: !T.Text,
  _pietyStars :: !Int,
  _hp, _mhp, _mmhp :: !Int,
  _mp, _mmp :: !Int,
  _ac, _ev, _sh :: !Int,
  _str, _mstr, _int, _mint, _dex, _mdex :: !Int,
  _xl, _progress :: !Int,
  _place :: !T.Text,
  _depth :: !Int,
  _gold :: !Int,
  _time :: !Int,
  _statuses :: !(H.HashMap T.Text Int)
  }

makeLenses ''Player

initialStatus :: Player
initialStatus = Player
                "Yak"
                ""
                0
                0 0 0
                0 0
                0 0 0
                0 0 0 0 0 0
                0 0
                "Nowhere"
                0
                0
                0
                H.empty

playerStatus :: R.Event t A.Value -> R.Behavior t Player
playerStatus input = R.accumB initialStatus $ fmap updateStatus input
  where updateStatus msg = execState $ do
          updateTextField "species" species
          updateTextField "god" god
          updateIntField "piety_rank" pietyStars
          updateIntField "hp" hp
          updateIntField "hp_max" mhp
          updateIntField "real_hp_max" mmhp
          updateIntField "mp" mp
          updateIntField "mp_max" mmp
          updateIntField "ac" ac
          updateIntField "ev" ev
          updateIntField "sh" sh
          updateIntField "str" str
          updateIntField "str_max" mstr
          updateIntField "int" int
          updateIntField "int_max" mint
          updateIntField "dex" dex
          updateIntField "dex_max" mdex
          updateIntField "xl" xl
          updateIntField "progress" progress
          updateTextField "place" place
          updateIntField "depth" depth
          updateIntField "gold" gold
          updateIntField "time" time
          F.mapM_ (statuses .=) (msg ^? key "status"._Array.to decodeStatuses)
          where updateIntField s l = F.mapM_ (l .=) (msg ^? key s._Integer.integral)
                updateTextField s l = F.mapM_ (l .=) (msg ^? key s._String)
                decodeStatuses = H.fromList . mapMaybe (\status -> liftA2 (,) (status ^? key "light"._String) (status ^? key "col"._Integer.integral)) . F.toList

hasStatus :: T.Text -> Player -> Bool
hasStatus status = (status `H.member`) . _statuses

isBerserk :: Player -> Bool
isBerserk = hasStatus "Berserk"

isConfused :: Player -> Bool
isConfused = hasStatus "Conf"

isExhausted :: Player -> Bool
isExhausted = hasStatus "Exh"

isSlow :: Player -> Bool
isSlow = hasStatus "Slow"

isPoisoned :: Player -> Bool
isPoisoned = hasStatus "Pois"

isMesmerised :: Player -> Bool
isMesmerised = hasStatus "Mesm"

isSilenced :: Player -> Bool
isSilenced = hasStatus "Sil"

hungerLevel :: Player -> HungerLevel
hungerLevel p = fromMaybe HS_SATIATED $
                fmap snd (find (flip hasStatus p . fst) (zip hungerStatuses [HS_FAINTING ..]))
  where hungerStatuses = ["Fainting", "Starving", "Near Starving",
                          "Very Hungry", "Hungry",
                          "Satiated" {- not really used -}, "Full",
                          "Very Full", "Engorged"]

canUseGodAbility :: T.Text -> Int -> Player -> Bool
canUseGodAbility g stars p =
  _god p == g && _pietyStars p >= stars
  && not (isBerserk p) && not (isConfused p) && not (isSilenced p)
  && hungerLevel p >= HS_NEAR_STARVING

canBerserk :: Player -> Bool
canBerserk p = not (isExhausted p) && not (isMesmerised p) && hungerLevel p >= HS_HUNGRY

canTrogBerserk :: Player -> Bool
canTrogBerserk p = canUseGodAbility "Trog" 1 p && canBerserk p

canTrogsHand :: Player -> Bool
canTrogsHand p = canUseGodAbility "Trog" 2 p

canBiA :: Player -> Bool
canBiA p = canUseGodAbility "Trog" 4 p

dlvl :: Player -> DungeonLevel
dlvl p = DungeonLevel (parseBranch (_place p)) (fixDepth $ _depth p)
  where fixDepth 0 = 1 -- places in single-level branches are reported as
                       -- e.g. Temple 0
        fixDepth d = d

canRead :: Player -> Bool
canRead p = not (isBerserk p) && not (isConfused p) && not (isSilenced p) && not (hasStatus "-Scroll" p)
