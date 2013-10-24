{-# LANGUAGE OverloadedStrings #-}

module Crawl.FloorItems (
  Items(..), knownItems, possiblyAny,
  trackFloorItems, scanFloorItems,
  wantItem,
  sacrificable
  ) where

import Control.Applicative ((<$>), (<*>), liftA2)

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Reactive.Banana as R
import qualified Data.Text as T

import Crawl.BananaUtils
import Crawl.Bindings
import Crawl.LevelInfo
import Crawl.Messages
import Crawl.Move

data Items = Empty
           | SingleItem T.Text
           | UnexploredStack T.Text
           | ExploredStack [T.Text]

knownItems :: Items -> [T.Text]
knownItems Empty = []
knownItems (SingleItem i) = [i]
knownItems (UnexploredStack i) = [i]
knownItems (ExploredStack is) = is

possiblyAny :: (T.Text -> Bool) -> Items -> Bool
possiblyAny _ Empty = False
possiblyAny f (SingleItem i) = f i
possiblyAny _ (UnexploredStack _) = True
possiblyAny f (ExploredStack is) = any f is

type FloorItems = H.HashMap Coord (Maybe Int, Items)

-- Store results of scanning for items.
-- At each square, we store the result of looking at that square with 'x',
-- along with the item tile data for that square at that time.
trackFloorItems :: R.Behavior t Coord ->
                   R.Behavior t LevelInfo ->
                   R.Behavior t MouseMode ->
                   R.Event t Message ->
                   R.Behavior t Coord ->
                   R.Event t Move ->
                   R.Event t MouseMode ->
                   R.Behavior t FloorItems
trackFloorItems cursor level inputModeB messages loc moves inputModeE =
  -- automatically clear out entries when we can see the item is gone
  liftA2 (H.intersectionWith (flip const)) (fmap _levelItemTiles level) $
  R.accumB H.empty $
  (handleItemMessages <$> cursor <*> level R.<@> (itemMessages $ R.whenE ((== MOUSE_MODE_TARGET) <$> inputModeB) messages))
  `R.union` (handleYouSeeHereMessages <$> loc <*> level R.<@> youSeeHereMessages messages)
  `R.union` (handleThingsThatAreHereMessages <$> loc <*> level R.<@>
             thingsThatAreHereMessages (R.whenE ((/= MOUSE_MODE_TARGET) <$> inputModeB) messages) (R.filterE (== MOUSE_MODE_COMMAND) inputModeE))
  `R.union` (handleMove <$> loc R.<@> moves)
  where handleItemMessages c ll imsgs = H.insert c (tile, items)
          where tile = H.lookup c (_levelItemTiles ll)
                items = case imsgs of
                  [] -> Empty
                  [item] -> SingleItem item
                  [item, "<lightgrey>There is something else lying underneath.<lightgrey>"] -> UnexploredStack item
                  -- is it possible that we're throwing away ExploredStack data?
                  _ -> error $ "strange itemMessages: " ++ show imsgs
        handleYouSeeHereMessages l ll item = H.insert l (H.lookup l (_levelItemTiles ll), SingleItem item)
        handleThingsThatAreHereMessages l ll items = H.insert l (H.lookup l (_levelItemTiles ll), ExploredStack items)
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

youSeeHereMessages :: R.Event t Message -> R.Event t T.Text
youSeeHereMessages = filterBy (T.stripPrefix "<lightgrey>You see here " . _msgText) . R.filterE ((== MSGCH_FLOOR_ITEMS) . _msgChannel)

thingsThatAreHereMessages :: R.Event t Message -> R.Event t a -> R.Event t [T.Text]
thingsThatAreHereMessages messages commandMode =
  R.filterJust . fst . R.mapAccum Nothing $ fmap handleMessage messages `R.union` fmap handleCommandMode commandMode
  where handleMessage Message { _msgChannel = MSGCH_FLOOR_ITEMS, _msgText = "<lightgrey>Things that are here:<lightgrey>" } Nothing = (Nothing, Just [])
        handleMessage Message { _msgChannel = MSGCH_PLAIN, _msgText = item } acc = (\x -> (x, x)) (fmap (++ [item]) acc)
        -- ugh. we have to do it this way because the floorItems must be updated *before* the input_mode = 0 event arrives,
        -- or we will choose a move based on the old value.
        handleMessage _ Nothing = (Nothing, Nothing)
        handleMessage _ (Just _) = error "unexpected message in thingsThatAreHereMessages"
        handleCommandMode _ _ = (Nothing, Nothing)


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
