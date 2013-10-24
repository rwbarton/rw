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
import Crawl.Messages

data Move = Go !Int !Int
          | Attack !Int !Int
          | GoDown
          | Rest
          | LongRest
          | PickUp (T.Text -> Bool)
          | AutoExplore
          | Pray
          | Eat InventorySlot
          | ScanItem !Int !Int


data SendOp a where
  Press :: T.Text -> SendOp ()
  ExpectPrompt :: T.Text -> SendOp ()
  SetPickupFunc :: (T.Text -> Bool) -> SendOp ()
  AnswerYesNo :: T.Text -> SendOp ()

type Send = Program SendOp

press :: T.Text -> Send ()
press = singleton . Press

expectPrompt :: T.Text -> Send ()
expectPrompt = singleton . ExpectPrompt

setPickupFunc :: (T.Text -> Bool) -> Send ()
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
moveProgram GoDown = press ">"
moveProgram Rest = press "."
moveProgram (PickUp f) = press "," >> setPickupFunc f
moveProgram LongRest = press "5"
moveProgram AutoExplore = press "o"
moveProgram Pray = press "p"
moveProgram (Eat slot) = do
  press "e"
  expectPrompt "<cyan>Eat which item? (<white>?<cyan> for menu, <white>Esc<cyan> to quit)<lightgrey>"
  press (T.singleton $ slotLetter slot)
moveProgram (ScanItem dx dy) = do
  -- todo: send this all in a single message
  press "x"
  let go 0 0 = return ()
      go rx ry = moveProgram (Go sx sy) >> go (rx - sx) (ry - sy)
        where sx = signum rx
              sy = signum ry
  go dx dy
  press "\ESC"

sendMoves :: R.Behavior t Move -> R.Event t Message -> R.Event t MouseMode -> (R.Event t Move, R.Event t T.Text)
sendMoves move messages inputModeChanged = R.split $ R.spill . fst $
                                           R.mapAccum (return ())
                                           ((handleMessage . _msgText <$> messages) `R.union` (handleInputMode <$> move R.<@> inputModeChanged))
  where handleMessage :: T.Text -> Send () -> ([Either Move T.Text], Send ())
        handleMessage message prog
          | "<cyan>Eat " `T.isPrefixOf` message && " (ye/n/q/i?)<lightgrey>" `T.isSuffixOf` message = ([Right "n"], prog) -- ew, don't eat off the ground
        handleMessage (T.stripPrefix "<cyan>Pick up " -> Just itemName) prog = ([], answerYesNo itemName >> prog)
        handleMessage "<cyan>Increase (S)trength, (I)ntelligence, or (D)exterity? <lightgrey>" prog
          = ([], press "s" >> prog)
        handleMessage message prog = (,) [] $ case view prog of
          ExpectPrompt m :>>= prog' | message == m -> prog' ()
          _ -> prog
        handleInputMode :: Move -> MouseMode -> Send () -> ([Either Move T.Text], Send ())
        handleInputMode mv MOUSE_MODE_COMMAND prog = first (Left mv :) $ case view prog of
          SetPickupFunc _ :>>= (view . ($ ()) -> Return ()) -> peel $ moveProgram mv
          Return () -> peel $ moveProgram mv
          _ -> error "desynchronized in sendMoves!"
        handleInputMode _ MOUSE_MODE_MORE prog = ([Right " "], prog)
        handleInputMode _ MOUSE_MODE_PROMPT prog = case peel prog of
          ([], _) -> error "out of input in sendMoves!"
          prog' -> prog'
        handleInputMode _ MOUSE_MODE_YESNO prog = case view prog of
          -- XXX this is such a hack
          AnswerYesNo item :>>= prog'@(view . ($ ()) -> SetPickupFunc f :>>= _) -> ([Right (if f item then "y" else "n")], prog' ())
          _ -> ([Right "Y"], prog)
        handleInputMode _ _ prog = ([], prog)

peel :: Send () -> ([Either Move T.Text], Send ())
peel prog = case view prog of
  Press t :>>= prog' -> let (outs, prog'') = peel (prog' ()) in (Right t : outs, prog'')
  _ -> ([], prog)
