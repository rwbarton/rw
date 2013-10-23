{-# LANGUAGE OverloadedStrings #-}

module Crawl.FloorItems (
  trackFloorItems, scanFloorItems,
  wantItem,
  sacrificable
  ) where

import Control.Applicative ((<$>), (<*>), liftA2)
import Data.Maybe (listToMaybe)

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Reactive.Banana as R
import qualified Data.Text as T

import Crawl.Bindings
import Crawl.LevelInfo
import Crawl.Messages
import Crawl.Move

type FloorItems = H.HashMap Coord (Maybe Int, Maybe T.Text)

-- Store results of scanning for items.
-- At each square, we store the result of looking at that square with 'x',
-- along with the item tile data for that square at that time.
trackFloorItems :: R.Behavior t Coord ->
                   R.Behavior t LevelInfo ->
                   R.Behavior t MouseMode ->
                   R.Event t Message ->
                   R.Behavior t Coord ->
                   R.Event t Move ->
                   R.Behavior t FloorItems
trackFloorItems cursor level inputModeB messages loc moves =
  -- automatically clear out entries when we can see the item is gone
  liftA2 (H.intersectionWith (flip const)) (fmap _levelItemTiles level) $
  R.accumB H.empty $
  (handleItemMessages <$> cursor <*> level R.<@> (itemMessages $ R.whenE (fmap (== MOUSE_MODE_TARGET) inputModeB) messages))
  `R.union` (handleMove <$> loc R.<@> moves)
  where handleItemMessages c ll items =
          case filter (/= "<lightgrey>There is something else lying underneath.<lightgrey>") items of
            several@(_ : _ : _) -> error $ "multiple floor item messages? " ++ show several
            atMostOne -> H.insert c (H.lookup c (_levelItemTiles ll), listToMaybe atMostOne)
        handleMove l Pray = H.delete l
        -- picking up items too, when implemented
        handleMove _ GoDown = const H.empty
        handleMove _ _ = id

scanFloorItems :: LevelInfo -> Coord -> FloorItems -> Maybe Move
scanFloorItems level (Coord lx ly) floorItems =
  -- find items in LOS whose tile data has changed
  case [ c | (c, t) <- H.toList (_levelItemTiles level), c `HS.member` _levelLOS level, fmap fst (H.lookup c floorItems) /= Just (Just t) ] of
    Coord x y : _ -> Just (ScanItem (x-lx) (y-ly))
    _ -> Nothing


itemMessages :: R.Event t Message -> R.Event t [T.Text]
itemMessages messages =
  R.filterJust $
  fst . R.mapAccum [] . fmap handleMessage $ messages
  where handleMessage Message { _msgChannel = MSGCH_PROMPT } _acc = (Nothing, [])
        handleMessage Message { _msgChannel = MSGCH_FLOOR_ITEMS, _msgText = t } acc = (Nothing, acc ++ [t])
        handleMessage Message { _msgChannel = MSGCH_EXAMINE_FILTER } acc = (Just acc, [])
        handleMessage _ acc = (Nothing, acc)


wantItem :: T.Text -> Bool
wantItem itemName
  = "gold piece" `T.isInfixOf` itemName ||
    "corpse" `T.isInfixOf` itemName && not ("rotting" `T.isInfixOf` itemName) ||
    isPermafood itemName

isPermafood :: T.Text -> Bool
isPermafood itemName =
  any (`T.isInfixOf` itemName) ["meat ration", "bread ration"]
  -- todo: add the rest; but watch out for "A sedimented orange potion."

sacrificable :: T.Text -> Bool
sacrificable itemName = "corpse" `T.isInfixOf` itemName && not ("rotting" `T.isInfixOf` itemName)
