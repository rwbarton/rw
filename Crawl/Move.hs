{-# LANGUAGE OverloadedStrings, GADTs, ViewPatterns #-}

module Crawl.Move (
  Move(..), sendMoves
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad.Operational (Program, singleton, view, ProgramViewT(..))

import qualified Data.Text as T
import qualified Reactive.Banana as R

import Crawl.Bindings
import Crawl.Inventory
import Crawl.Item
import Crawl.Messages
import Crawl.Menu
import Crawl.ParseItem

data Move = Go !Int !Int
          | Attack !Int !Int
          | Berserk
          | TrogsHand
          | BiA
          | BurnBooks
          | GoUp
          | GoDown
          | Rest
          | LongRest
          | PickUp (Item -> Bool)
          | AutoExplore
          | AutoFight
          | AutoPickup
          | Butcher
          | Pray
          | Drop InventorySlot
          | Eat InventorySlot
          | Wield InventorySlot
          | Wear InventorySlot
          | TakeOff InventorySlot
          | Quaff InventorySlot
          | Read InventorySlot
          | BlinkTo InventorySlot !Int !Int
          | ScanBigStack
          | ScanItem !Int !Int
          | LookHere
          | Dump


data SendOp a where
  Press :: T.Text -> SendOp ()
  ExpectPrompt :: T.Text -> SendOp ()
  ExpectMenu :: (T.Text -> Bool) -> SendOp ()
  ExpectBlink :: SendOp ()
  SetPickupFunc :: (Item -> Bool) -> SendOp ()
  AnswerYesNo :: T.Text -> SendOp ()

type Send = Program SendOp

press :: T.Text -> Send ()
press = singleton . Press

_expectPrompt :: T.Text -> Send ()
_expectPrompt = singleton . ExpectPrompt

expectMenu :: (T.Text -> Bool) -> Send ()
expectMenu = singleton . ExpectMenu

expectBlink :: Send ()
expectBlink = singleton ExpectBlink

setPickupFunc :: (Item -> Bool) -> Send ()
setPickupFunc = singleton . SetPickupFunc

answerYesNo :: T.Text -> Send ()
answerYesNo = singleton . AnswerYesNo


moveProgram :: Move -> Send ()
moveProgram (Go dx dy) = press $ case (dx, dy) of
  (-1,  0) -> "h"
  ( 0,  1) -> "j"
  ( 0, -1) -> "k"
  ( 1,  0) -> "l"
  (-1, -1) -> "y"
  ( 1, -1) -> "u"
  (-1,  1) -> "b"
  ( 1,  1) -> "n"
  _        -> error "tried to make illegal move"
moveProgram (Attack dx dy) = press $ case (dx, dy) of
  (-1,  0) -> "\8"
  ( 0,  1) -> "\10"
  ( 0, -1) -> "\11"
  ( 1,  0) -> "\12"
  (-1, -1) -> "\25"
  ( 1, -1) -> "\21"
  (-1,  1) -> "\2"
  ( 1,  1) -> "\14"
  _        -> error "tried to make illegal attack"
moveProgram Berserk = useAbility "a"
moveProgram TrogsHand = useAbility "b"
moveProgram BiA = useAbility "c"
moveProgram BurnBooks = useAbility "d"
moveProgram GoUp = press "<"
moveProgram GoDown = press ">"
moveProgram Rest = press "."
moveProgram (PickUp f) = press "," >> setPickupFunc f
moveProgram LongRest = press "5"
moveProgram AutoExplore = press "o"
moveProgram AutoFight = press "\t"
moveProgram AutoPickup = press "\1"
moveProgram Butcher = press "c"
moveProgram Pray = press "p"
moveProgram (Drop slot) = do
  press "d"
  expectMenu ("<white>Drop what? " `T.isPrefixOf`)
  press (T.singleton $ slotLetter slot)
  press "\r"
moveProgram (Eat slot) = do
  press "e"
  expectMenu (== "<white>Eat which item?")
  press (T.singleton $ slotLetter slot)
moveProgram (Wield slot) = do
  press "w"
  expectMenu (== "<white>Wield which item (- for none, * to show all)?")
  press (T.singleton $ slotLetter slot)
moveProgram (Wear slot) = do
  press "W"
  expectMenu (== "<white>Wear which item?")
  press (T.singleton $ slotLetter slot)
moveProgram (TakeOff slot) = do
  press "T"
  expectMenu (== "<white>Take off which item?")
  press (T.singleton $ slotLetter slot)
moveProgram (Quaff slot) = do
  press "q"
  expectMenu (== "<white>Drink which item?")
  press (T.singleton $ slotLetter slot)
moveProgram (Read slot) = do
  press "r"
  expectMenu (== "<white>Read which item?")
  press (T.singleton $ slotLetter slot)
moveProgram (BlinkTo slot dx dy) = do
  moveProgram (Read slot)
  expectBlink
  waltz dx dy
  press "."
moveProgram ScanBigStack = moveProgram (PickUp (const False))
moveProgram (ScanItem dx dy) = do
  -- todo: send this all in a single message
  press "x"
  waltz dx dy
  press "\ESC"
moveProgram LookHere = press ";"
moveProgram Dump = press "#"

waltz :: Int -> Int -> Send ()
waltz 0 0 = return ()
waltz rx ry = moveProgram (Go sx sy) >> waltz (rx - sx) (ry - sy)
  where sx = signum rx
        sy = signum ry

useAbility :: T.Text -> Send ()
useAbility a = do
  press "a"
  expectMenu (T.isPrefixOf "<white>  Ability - do what?")
  press a


sendMoves :: R.Behavior t Move -> R.Event t Message -> R.Event t MouseMode -> R.Event t Menu -> (R.Event t Move, R.Event t T.Text)
sendMoves move messages inputModeChanged menu
  = R.split $ R.spill . fst $
    R.mapAccum (return ())
    ((handleMessage . _msgText <$> messages) `R.union` (handleInputMode <$> move R.<@> inputModeChanged)
    `R.union` (handleMenu <$> menu))
  where handleMessage :: T.Text -> Send () -> ([Either Move T.Text], Send ())
        handleMessage message prog
          | "<cyan>Eat " `T.isPrefixOf` message && " (ye/n/q/i?)<lightgrey>" `T.isSuffixOf` message = ([Right "n"], prog) -- ew, don't eat off the ground
        handleMessage
          (T.stripPrefix "<cyan>Pick up " ->
           Just (T.stripSuffix "? ((y)es/(n)o/(a)ll/(m)enu/*?g,/q)<lightgrey>" -> Just itemName)) prog =
            ([], answerYesNo itemName >> prog)
        handleMessage "<cyan>Increase (S)trength, (I)ntelligence, or (D)exterity? <lightgrey>" prog
          = ([], press "s" >> prog)
        handleMessage message prog
          | "<cyan>Butcher " `T.isPrefixOf` message = ([Right "a"], prog)
        handleMessage "<cyan>What kind of item would you like to acquire? (\\ to view known items)<lightgrey>" prog = ([], press "b" >> prog)
        handleMessage "<cyan>Blink to where?<lightgrey>" prog = case view prog of
          ExpectBlink :>>= prog' -> peel (prog' ())
          Return _ -> ([Right "\ESC"], prog)
          _ -> error "bad prog in handleMessage(blink)"
        handleMessage message (view -> ExpectPrompt m :>>= prog') | message == m = ([], prog' ())
        handleMessage _ prog = ([], prog)

        handleInputMode :: Move -> MouseMode -> Send () -> ([Either Move T.Text], Send ())
        handleInputMode mv MOUSE_MODE_COMMAND prog = first (Left mv :) $ case view prog of
          -- XXX Mega Hack
          _ -> peel $ moveProgram mv
{-
          SetPickupFunc _ :>>= (view . ($ ()) -> Return ()) -> peel $ moveProgram mv
          Return () -> peel $ moveProgram mv
          _ -> error "desynchronized in sendMoves!"
-}
        handleInputMode _ MOUSE_MODE_MORE prog = ([Right " "], prog)
        handleInputMode _ MOUSE_MODE_PROMPT prog = case peel prog of
          ([], _) -> error "out of input in sendMoves!"
          prog' -> prog'
        handleInputMode _ MOUSE_MODE_YESNO prog = case view prog of
          -- XXX this is such a hack
          AnswerYesNo item :>>= prog'@(view . ($ ()) -> SetPickupFunc f :>>= _) -> ([Right (if f (parseItem item) then "y" else "n")], prog' ())
          _ -> ([Right "Y"], prog)
        handleInputMode _ _ prog = ([], prog)
        handleMenu :: Menu -> Send () -> ([Either Move T.Text], Send ())
        handleMenu (_menuTag -> "shop") prog = ([], press "\ESC" >> prog)
        handleMenu (_menuTag -> "skills") prog = ([Right "\r"], prog)
        handleMenu (_menuTitle -> title) prog
          | "<white>Inventory: " `T.isPrefixOf` title = ([Right "\ESC"], prog)
        handleMenu mn prog
          | _menuTitle mn `elem` ["<white>Identify which item? (\\ to view known items)",
                                  "<white>Enchant which item?", "<white>Enchant which weapon?",
                                  "<white>Brand which weapon?"]
          = ([Right (anyItem mn)], prog)
        handleMenu (_menuTitle -> title) prog = case view prog of
          ExpectMenu f :>>= prog' | f title -> peel (prog' ())
          _ -> ([], prog)

peel :: Send () -> ([Either Move T.Text], Send ())
peel prog = case view prog of
  Press t :>>= prog' -> let (outs, prog'') = peel (prog' ()) in (Right t : outs, prog'')
  _ -> ([], prog)
