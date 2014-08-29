{-# LANGUAGE OverloadedStrings #-}

module Crawl.FloorItems (
  Items(..), knownItems, possiblyAny,
  trackFloorItems, scanFloorItems,
  wantItem, wantItemPickup,
  sacrificable
  ) where

import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Monad (mplus)

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Reactive.Banana as R
import qualified Data.Text as T

import Crawl.BananaUtils
import Crawl.Bindings
import Crawl.Equipment
import Crawl.Inventory
import Crawl.LevelInfo
import Crawl.Messages
import Crawl.Move
import Crawl.ParseItem

data Items = Empty
           | SingleItem T.Text
           | UnexploredStack T.Text
           | ExploredStack [T.Text]
           | BigStack

knownItems :: Items -> [T.Text]
knownItems Empty = []
knownItems (SingleItem i) = [i]
knownItems (UnexploredStack i) = [i]
knownItems (ExploredStack is) = is
knownItems BigStack = []

possiblyAny :: (T.Text -> Bool) -> Items -> Bool
possiblyAny _ Empty = False
possiblyAny f (SingleItem i) = f i
possiblyAny _ (UnexploredStack _) = True
possiblyAny f (ExploredStack is) = any f is
possiblyAny _ BigStack = False

type FloorItems = H.HashMap Coord (Maybe Int, Items)

-- Store results of scanning for items.
-- At each square, we store the result of looking at that square with 'x',
-- along with the item tile data for that square at that time.
trackFloorItems :: R.Behavior t Coord ->
                   R.Behavior t LevelInfo ->
                   R.Behavior t MouseMode ->
                   R.Event t Message ->
                   R.Behavior t (Int, Move) ->
                   R.Behavior t Coord ->
                   R.Event t Move ->
                   R.Event t MouseMode ->
                   R.Behavior t FloorItems
trackFloorItems cursor level inputModeB messages0 lastMove loc moves inputModeE =
  -- automatically clear out entries when we can see the item is gone
  liftA2 (H.intersectionWith (flip const)) (fmap _levelItemTiles level) $
  R.accumB H.empty $
  (handleItemMessages <$> cursor <*> level R.<@> (itemMessages $ R.whenE ((== MOUSE_MODE_TARGET) <$> inputModeB) messages))
  `R.union` (handleYouSeeHereMessages <$> loc <*> level R.<@> youSeeHereMessages messages)
  `R.union` (handleThingsThatAreHereMessages <$> loc <*> level R.<@>
             thingsThatAreHereMessages (R.whenE ((/= MOUSE_MODE_TARGET) <$> inputModeB) messages) (R.filterE (== MOUSE_MODE_COMMAND) inputModeE))
  `R.union` (handleManyItemsHereMessages <$> loc <*> level R.<@> R.filterE ((== "<lightgrey>There are many items here.<lightgrey>") . _msgText) messages)
  `R.union` (handleScanBigStackMessages <$> loc <*> level R.<@> (filterBy (T.stripSuffix "? ((y)es/(n)o/(a)ll/(m)enu/*?g,/q)<lightgrey>") . filterBy (T.stripPrefix "<cyan>Pick up ") . fmap _msgText) scanBigStackMessages)
  `R.union` (handleMove <$> loc R.<@> moves)
  where messages = R.whenE ((\(_, m) -> case m of AutoExplore -> False; _ -> True) <$> lastMove) messages0
        scanBigStackMessages = R.whenE ((\(_, m) -> case m of ScanBigStack -> True; _ -> False) <$> lastMove) messages0
        handleItemMessages c ll imsgs = H.insert c (tile, items)
          where tile = H.lookup c (_levelItemTiles ll)
                items = case imsgs of
                  [] -> Empty
                  [item] -> SingleItem item
                  [item, "<lightgrey>There is something else lying underneath.<lightgrey>"] -> UnexploredStack item
                  -- is it possible that we're throwing away ExploredStack data?
                  _ -> error $ "strange itemMessages: " ++ show imsgs
        handleYouSeeHereMessages l ll item = H.insert l (H.lookup l (_levelItemTiles ll), SingleItem item)
        handleThingsThatAreHereMessages l ll items = H.insert l (H.lookup l (_levelItemTiles ll), ExploredStack items)
        handleManyItemsHereMessages l ll _ = H.insert l (H.lookup l (_levelItemTiles ll), BigStack)
        handleScanBigStackMessages l ll m old = H.insert l (H.lookup l (_levelItemTiles ll), newItems) old
          where newItems = case H.lookup l old of
                  Just (_, BigStack) -> ExploredStack [m]
                  Just (_, ExploredStack e) -> ExploredStack (e ++ [m])
                  _ -> error "handleScanBigStackMessages: unexpected previous floorItems contents"
                  -- could potentially do something cleverer here:
                  -- accumulate the item list and only update on "Okay, then.",
                  -- but this should be fine
        handleMove l Butcher = H.delete l
        handleMove l Pray = H.delete l
        handleMove l (PickUp _) = H.delete l
        handleMove _ GoDown = const H.empty
        handleMove _ _ = id

scanFloorItems :: LevelInfo -> Coord -> FloorItems -> Maybe Move
scanFloorItems level l@(Coord lx ly) floorItems = scanBigStackHere `mplus` scanNearby
  where scanBigStackHere = -- examine big stack if we're on one
          case H.lookup l floorItems of
            Just (_, BigStack) -> Just ScanBigStack
            _ -> Nothing
        scanNearby = -- find items in LOS whose tile data has changed
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

youSeeHereMessages :: R.Event t Message -> R.Event t T.Text
youSeeHereMessages = filterBy (T.stripPrefix "<lightgrey>You see here " . _msgText) . R.filterE ((== MSGCH_FLOOR_ITEMS) . _msgChannel)

thingsThatAreHereMessages :: R.Event t Message -> R.Event t a -> R.Event t [T.Text]
thingsThatAreHereMessages messages commandMode =
  R.filterJust . fst . R.mapAccum Nothing $ fmap handleMessage messages `R.union` fmap handleCommandMode commandMode
  where handleMessage Message { _msgChannel = MSGCH_FLOOR_ITEMS, _msgText = "<lightgrey>Things that are here:<lightgrey>" } Nothing = (Nothing, Just [])
        handleMessage Message { _msgChannel = MSGCH_PLAIN, _msgText = "<lightgrey>There are no objects that can be picked up here.<lightgrey>" } Nothing = (Nothing, Just [])
        handleMessage Message { _msgChannel = MSGCH_PLAIN, _msgText = item } acc = (\x -> (x, x)) (fmap (++ [item]) acc)
        -- ugh. we have to do it this way because the floorItems must be updated *before* the input_mode = 0 event arrives,
        -- or we will choose a move based on the old value.
        handleMessage _ Nothing = (Nothing, Nothing)
        handleMessage _ (Just _) = error "unexpected message in thingsThatAreHereMessages"
        handleCommandMode _ _ = (Nothing, Nothing)


-- The rest should probably get split out into its own module.

wantItem :: Inventory -> T.Text -> Bool
wantItem inv itemName
  = maybe False (wantItemPickup inv) (parseItemType itemName) ||
    sacrificable itemName
    -- "corpse" `T.isInfixOf` itemName && not ("rotting" `T.isInfixOf` itemName)

wantItemPickup :: Inventory -> ItemType -> Bool
wantItemPickup _ ItemGold = True
wantItemPickup _ (ItemFood _) = True
wantItemPickup _ (ItemPotion _) = True
wantItemPickup _ (ItemScroll _) = True
wantItemPickup inv item = isEquipmentUpgrade inv item

sacrificable :: T.Text -> Bool
sacrificable itemName = "corpse" `T.isInfixOf` itemName && not ("rotting" `T.isInfixOf` itemName)
