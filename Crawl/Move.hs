{-# LANGUAGE OverloadedStrings, GADTs #-}

module Crawl.Move (
  Move(..), sendMoves
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Operational (Program, singleton, view, ProgramViewT(..))

import qualified Data.Text as T
import qualified Reactive.Banana as R

import Crawl.Bindings
import Crawl.Inventory

data Move = Go !Int !Int
          | Attack !Int !Int
          | GoDown
          | Rest
          | Eat InventorySlot


data SendOp a where
  Press :: T.Text -> SendOp ()
  ExpectPrompt :: T.Text -> SendOp ()

type Send = Program SendOp

press :: T.Text -> Send ()
press = singleton . Press

expectPrompt :: T.Text -> Send ()
expectPrompt = singleton . ExpectPrompt


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
moveProgram (Eat slot) = do
  press "e"
  expectPrompt "<cyan>Eat which item? (<white>?<cyan> for menu, <white>Esc<cyan> to quit)<lightgrey>"
  press (T.singleton $ slotLetter slot)

sendMoves :: R.Behavior t Move -> R.Event t T.Text -> R.Event t MouseMode -> R.Event t T.Text
sendMoves move messages inputModeChanged = R.spill . fst $
                                           R.mapAccum (return ())
                                           ((handleMessage <$> messages) `R.union` (handleInputMode <$> move R.<@> inputModeChanged))
  where handleMessage :: T.Text -> Send () -> ([T.Text], Send ())
        handleMessage message prog
          | "<cyan>Eat " `T.isPrefixOf` message && " (ye/n/q/i?)<lightgrey>" `T.isSuffixOf` message = (["n"], prog) -- ew, don't eat off the ground
        handleMessage "<cyan>Increase (S)trength, (I)ntelligence, or (D)exterity? <lightgrey>" prog
          = ([], press "s" >> prog)
        handleMessage message prog = (,) [] $ case view prog of
          ExpectPrompt m :>>= prog' | message == m -> prog' ()
          _ -> prog
        handleInputMode :: Move -> MouseMode -> Send () -> ([T.Text], Send ())
        handleInputMode mv MOUSE_MODE_COMMAND prog = case view prog of
          Return () -> peel $ moveProgram mv
          _ -> error "desynchronized in sendMoves!"
        handleInputMode _ MOUSE_MODE_MORE prog = ([" "], prog)
        handleInputMode _ MOUSE_MODE_PROMPT prog = case peel prog of
          ([], _) -> error "out of input in sendMoves!"
          prog' -> prog'
        handleInputMode _ MOUSE_MODE_YESNO prog = (["Y"], prog)
        handleInputMode _ _ prog = ([], prog)

peel :: Send () -> ([T.Text], Send ())
peel prog = case view prog of
  Press t :>>= prog' -> let (outs, prog'') = peel (prog' ()) in (t : outs, prog'')
  _ -> ([], prog)
