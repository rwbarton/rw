{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Crawl.Play (
  play
  ) where

import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Monad (forever, guard)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Exit (exitWith, ExitCode(ExitSuccess))

import Control.Concurrent.Chan.Split (InChan, OutChan, writeChan, readChan)
import Control.Lens ((^..), (^?), at, to, traverse, enum)
import Control.Lens.Aeson (_Integer, _String, _Array, key)
import Data.Bits.Lens (bitAt)
import Numeric.Lens (integral)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Reactive.Banana as R
import qualified Reactive.Banana.Frameworks as R
import qualified Data.Text as T

import Crawl.BadForms
import Crawl.BananaUtils
import Crawl.Bindings
import Crawl.Equipment
import Crawl.Explore
import Crawl.FloorItems
import Crawl.Inventory
import Crawl.LevelInfo
import Crawl.Messages
import Crawl.Move
import Crawl.Status

play :: OutChan A.Object -> InChan A.Object -> IO ()
play recvChan sendChan = do
  (recvHandler, recvFire) <- R.newAddHandler
  let sendHandler = writeChan sendChan
  network <- R.compile $ setupNetwork recvHandler sendHandler
  R.actuate network
  forever $ do
    msg <- readChan recvChan
    mapM_ recvFire $ demultiplex msg

setupNetwork :: R.Frameworks t => R.AddHandler A.Value -> (A.Object -> IO ()) -> R.Moment t ()
setupNetwork recvHandler sendHandler = do
  input <- R.fromAddHandler recvHandler
  let demultiplexed = input

      ping = R.filterE (\msg -> msg ^? key "msg" == Just "ping") demultiplexed
      pong = fmap (const $ H.fromList [("msg", "pong")]) ping

      messages = messagesOf demultiplexed

      inputModeEvents = filterBy (\msg -> do
                                     guard $ msg ^? key "msg" == Just "input_mode"
                                     msg ^? key "mode"._Integer.integral.enum) demultiplexed
      inputModeB = R.stepper MOUSE_MODE_NORMAL inputModeEvents
      inputModeChanged = R.filterApply (fmap (/=) inputModeB) inputModeEvents

      inventoryMore = R.filterE (\msg -> msg ^? key "msg" == Just "menu" &&
                                         msg ^? key "tag" == Just "inventory" &&
                                         msg ^? key "flags"._Integer.bitAt 12 == Just True)
                      demultiplexed

      goodbye = R.filterE (\msg -> msg ^? key "msg" == Just "txt" &&
                                   msg ^? key "id"  == Just "crt" &&
                                   msg ^? key "lines".key "0"._String.to (T.isInfixOf "Goodbye,") == Just True)
                demultiplexed
      stillAlive = R.stepper True $ fmap (const False) goodbye

      gameOver = R.filterE (\msg -> msg ^? key "msg" == Just "game_ended") demultiplexed

      player = playerStatus $ R.filterE (\msg -> msg ^? key "msg" == Just "player") demultiplexed

      level = levelInfo $ R.filterE (\msg -> msg ^? key "msg" == Just "map") demultiplexed
      loc = R.stepper (Coord 0 0) $
            filterBy (\msg -> do
                         guard  $ msg ^? key "msg" == Just "player"
                         x <- msg ^? key "pos".key "x"._Integer.integral
                         y <- msg ^? key "pos".key "y"._Integer.integral
                         return (Coord x y)) demultiplexed

      cursor = R.stepper (Coord 0 0) $
               filterBy (\msg -> do
                            guard  $ msg ^? key "msg" == Just "cursor"
                            x <- msg ^? key "loc".key "x"._Integer.integral
                            y <- msg ^? key "loc".key "y"._Integer.integral
                            return (Coord x y)) demultiplexed

      inv = inventory $ R.filterE (\msg -> msg ^? key "msg" == Just "player") demultiplexed
      equip = equipment $ R.filterE (\msg -> msg ^? key "msg" == Just "player") demultiplexed

      needRest = (\p -> _hp p < _mhp p) <$> player
      rest = (\nr lm t -> if nr
                          then case lm of
                            (t', LongRest) | t' == t -> Just Rest
                            _ -> Just LongRest
                          else Nothing) <$> needRest <*> lastMove <*> (_time <$> player)

      eat = (\p i -> listToMaybe $ do
                guard $ hungerLevel p < 4
                (slot, itemType -> ItemFood _) <- M.toList i
                return $ Eat slot
                ) <$> player <*> inv

      floorItems = trackFloorItems cursor level inputModeB messages lastMove loc moves inputModeChanged

      pickup =
        (\l i wi -> do
            (_, is) <- H.lookup l i
            guard $ any wi . knownItems $ is
            return (PickUp wi)) <$> loc <*> floorItems <*> (fmap wantItem inv)

      corpses = fmap (HS.fromList . H.keys . H.filter (any sacrificable . knownItems . snd)) floorItems
      sac = (\l c -> guard (HS.member l c) >> Just Pray) <$> loc <*> corpses
      -- 'loot' is responsible for getting us to the corpse

      berserk = (\p l -> guard (2 * _hp p < _mhp p && canBerserk p)
                         >> guard (not . HS.null $ _levelLOS l `HS.intersection` HS.fromList (H.keys (_levelMonsters l)))
                         >> Just Berserk) <$> player <*> level

      trogsHand = (\p -> guard (isPoisoned p && _hp p <= 2 && not (hasStatus "Regen MR" p) && canTrogsHand p) >> Just TrogsHand) <$> player

      exploreWithAuto =
        (\ll l lm t ->
          case explore ll l of
            Nothing -> Nothing
            Just e -> case lm of
              (t', AutoExplore) | t' == t -> Just e
              _ -> Just AutoExplore) <$> level <*> loc <*> lastMove <*> (_time <$> player)

      move = foldr (liftA2 (flip fromMaybe)) (R.pure Rest) $ map (fmap filterLegalInForm player <*>) [
        scanFloorItems <$> level <*> loc <*> floorItems,
        berserk,
        trogsHand,
        kill <$> level <*> loc,
        eat,
        sac,
        rest,
        pickup,
        loot <$> level <*> loc <*> floorItems <*> inv, -- should probably produce set of things we want here, not in Explore
        upgradeEquipment <$> inv <*> equip,
        dropJunkEquipment <$> inv <*> equip,
        exploreWithAuto,
        descend <$> level <*> loc
        ]
      (moves, goText) = sendMoves move (R.whenE (fmap (/= MOUSE_MODE_TARGET) inputModeB) messages) (R.whenE stillAlive inputModeChanged)
      lastMove = R.stepper (0, GoDown) {- whatever -} $ (,) <$> (_time <$> player) R.<@> moves
      clearText = fmap (const " ") inventoryMore `R.union`
                  fmap (const " ") goodbye

      outputText = goText `R.union` clearText
      output = pong `R.union` fmap textInput outputText
  R.reactimate $ fmap print messages
  R.reactimate $ fmap print outputText
  R.reactimate $ fmap sendHandler output
  R.reactimate $ fmap (const $ exitWith ExitSuccess) gameOver

demultiplex :: A.Object -> [A.Value]
demultiplex msg = msg^..at "msgs".traverse._Array.traverse

textInput :: T.Text -> A.Object
textInput text = H.fromList [
  ("msg", "input"),
  ("text", A.String text)
  ]
