{-# LANGUAGE OverloadedStrings, DeriveFunctor, ViewPatterns #-}

module Crawl.FloorItems (
  SquareItems(..), knownItems, possiblyAny,
  FloorItems, trackFloorItems, scanFloorItems,
  wantItem, wantItemPickup,
  butcherable, isBook, isRune
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
import Crawl.Item
import Crawl.LevelInfo
import Crawl.Messages
import Crawl.Move
import Crawl.ParseItem

data SquareItems item =
  Empty
  | SingleItem item
  | UnexploredStack item
  | ExploredStack [item]
  | BigStack
  deriving (Functor)

knownItems :: SquareItems item -> [item]
knownItems Empty = []
knownItems (SingleItem i) = [i]
knownItems (UnexploredStack i) = [i]
knownItems (ExploredStack is) = is
knownItems BigStack = []

possiblyAny :: (item -> Bool) -> SquareItems item -> Bool
possiblyAny _ Empty = False
possiblyAny f (SingleItem i) = f i
possiblyAny _ (UnexploredStack _) = True
possiblyAny f (ExploredStack is) = any f is
possiblyAny _ BigStack = False

type FloorItems = H.HashMap Coord (Maybe Int, SquareItems Item)

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
  fmap ((fmap . fmap . fmap) parseItem) $
  -- automatically clear out entries when we can see the item is gone
  -- XXX wouldn't it be better to remove the entry from the accumB?
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
        handleMove _ BurnBooks = const H.empty -- trigger rescan; XXX just delete books
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

        -- In 0.17 we started getting these here frequently, not sure why,
        -- but it seems safe to ignore them
        handleMessage Message { _msgChannel = MSGCH_DURATION } _ = (Nothing, Nothing)
        handleMessage Message { _msgChannel = MSGCH_FOOD } _ = (Nothing, Nothing)
        handleMessage Message { _msgChannel = MSGCH_SOUND } _ = (Nothing, Nothing)
        handleMessage Message { _msgChannel = MSGCH_ROTTEN_MEAT } _ = (Nothing, Nothing)
        handleMessage Message { _msgChannel = MSGCH_TIMED_PORTAL } _ = (Nothing, Nothing)
        handleMessage Message { _msgChannel = MSGCH_MONSTER_WARNING } _ = (Nothing, Nothing)
        handleMessage _ Nothing = (Nothing, Nothing)
        handleMessage _ (Just _) = error "unexpected message in thingsThatAreHereMessages"
        handleCommandMode _ _ = (Nothing, Nothing)


-- The rest should probably get split out into its own module.

wantItem :: Bool -> Inventory -> Item -> Bool
wantItem corpsesOnly inv item
  = not corpsesOnly && wantItemPickup inv item || butcherable item

wantItemPickup :: Inventory -> Item -> Bool
wantItemPickup inv item = case itemData item of
  ItemGold -> False
  ItemFood _ -> True

  ItemPotion Nothing -> True
  ItemPotion (Just POT_CURING) -> True
  ItemPotion (Just POT_HEAL_WOUNDS) -> True
  ItemPotion (Just POT_HASTE) -> True
  ItemPotion (Just POT_MIGHT) -> True
  ItemPotion (Just POT_AGILITY) -> True
  ItemPotion (Just POT_RESISTANCE) -> True
  ItemPotion (Just POT_EXPERIENCE) -> True
  ItemPotion (Just POT_BENEFICIAL_MUTATION) -> True

  ItemScroll Nothing -> True
  ItemScroll (Just SCR_REMOVE_CURSE) -> True
  ItemScroll (Just SCR_ENCHANT_WEAPON) -> True
  ItemScroll (Just SCR_ENCHANT_ARMOUR) -> True
  ItemScroll (Just SCR_TELEPORTATION) -> True
  ItemScroll (Just SCR_MAGIC_MAPPING) -> True
  ItemScroll (Just SCR_ACQUIREMENT) -> True

  ItemMiscellany (Just MISC_RUNE_OF_ZOT) -> True
  _ -> isEquipmentUpgrade inv item

butcherable :: Item -> Bool
butcherable (itemData -> ItemCorpse _ False) = True
butcherable _ = False

isBook :: Item -> Bool
isBook (itemData -> ItemBook {}) = True
isBook _ = False

isRune :: Item -> Bool
isRune (itemData -> ItemMiscellany (Just MISC_RUNE_OF_ZOT)) = True
isRune _ = False
