{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Crawl.Play (
  play
  ) where

import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Monad (forever, guard)
import Data.List (intersect)
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
                (slot, item@(itemType -> ItemFood foodType)) <- M.toList i
                if foodType == FOOD_CHUNK
                  then guard $ not (itemColour item `elem` [10, 12, 8])
                  else guard $ hungerLevel p < 3
                return $ Eat slot
                ) <$> player <*> inv

      floorItems = trackFloorItems cursor level inputModeB messages lastMove loc moves inputModeChanged

      pickup =
        (\l i wi -> do
            (_, is) <- H.lookup l i
            guard $ any wi . knownItems $ is
            return (PickUp wi)) <$> loc <*> floorItems <*> (fmap wantItem inv)

      corpses = fmap (HS.fromList . H.keys . H.filter (any sacrificable . knownItems . snd)) floorItems
      useCorpse = (\l c p ->
                    guard (HS.member l c) >>
                    return (if hungerLevel p < 4 then Butcher else Pray)) <$> loc <*> corpses <*> player
      -- 'loot' is responsible for getting us to the corpse

      berserk = (\p l -> guard (2 * _hp p < _mhp p && canBerserk p)
                         >> guard (not . HS.null $ _levelLOS l `HS.intersection` HS.fromList (H.keys (_levelMonsters l)))
                         >> Just Berserk) <$> player <*> level

      trogsHand =
        (\p l -> do
            guard (not (hasStatus "Regen MR" p) && canTrogsHand p)
            let monstersInView = [ _monsterType mon | sq <- HS.toList $ _levelLOS l, Just mon <- return (H.lookup sq (_levelMonsters l)) ]
            guard $ isPoisoned p && _hp p <= 2 || not (null $ monstersInView `intersect` trogsHandMonsters)
            return TrogsHand) <$> player <*> level
        where trogsHandMonsters = -- http://www.reddit.com/r/roguelikes/comments/1eq3g6/dcss_advice_for_mibe/ca2pwni
                [MONS_WIZARD, MONS_OGRE_MAGE, MONS_DEEP_ELF_MAGE, MONS_EROLCHA, MONS_DEEP_ELF_SORCERER,
                 MONS_DEEP_ELF_DEMONOLOGIST, MONS_ORC_SORCERER, MONS_SPHINX, MONS_GREAT_ORB_OF_EYES, MONS_GOLDEN_EYE,
                 MONS_LICH, MONS_ANCIENT_LICH, MONS_RUPERT, MONS_NORRIS, MONS_AIZUL, MONS_MENNAS, MONS_LOUISE,
                 MONS_JORGRUN, MONS_DRACONIAN_SHIFTER, MONS_CACODEMON, MONS_PANDEMONIUM_LORD, MONS_ERESHKIGAL] ++
                [MONS_SIGMUND, MONS_GRINDER]

      exploreWithAuto =
        (\ll l lm t ->
          case explore ll l of
            Nothing -> Nothing
            Just e -> case lm of
              (t', AutoExplore) | t' == t -> Just e
              _ -> Just AutoExplore) <$> level <*> loc <*> lastMove <*> (_time <$> player)

      killWithTab =
        (\ll l p lm t ->
          case kill ll l p of
            Nothing -> Nothing
            Just m -> case (m, lm) of
              (_, (t', AutoFight)) | t' == t -> Just m
              (Attack _ _, _) -> Just AutoFight
              _ -> Just m) <$> level <*> loc <*> player <*> lastMove <*> (_time <$> player)

      invisibleMonsters =
        R.stepper False $
        (const True <$> R.filterE ((== "<lightred>Deactivating autopickup; reactivate with <white>Ctrl-A<lightred>.<lightgrey>") . _msgText) messages)
        `R.union` (const False <$> R.filterE ((== "<lightred>Reactivating autopickup.<lightgrey>") . _msgText) messages)
      killInvisible = (\ims u -> guard ims >> Just (Attack (dxs !! u) (dys !! u))) <$> invisibleMonsters <*> R.stepper 0 (randomize (0, 7) demultiplexed)
        where dxs = [-1, -1, -1, 0, 1, 1, 1, 0]
              dys = [-1, 0, 1, 1, 1, 0, -1, -1]

      beenTo = fmap (flip HS.member) $
        R.accumB HS.empty $
        (HS.insert . _place <$> player) R.<@ moves


      move = foldr (liftA2 (flip fromMaybe)) (R.pure Rest) $ map (fmap filterLegalInForm player <*>) [
        scanFloorItems <$> level <*> loc <*> floorItems,
        berserk,
        trogsHand,
        killInvisible,
        killWithTab,
        eat,
        useCorpse,
        rest,
        pickup,
        enterBranches <$> level <*> loc <*> beenTo,
        loot <$> level <*> loc <*> floorItems <*> inv, -- should probably produce set of things we want here, not in Explore
        upgradeEquipment <$> inv <*> equip,
        dropJunkEquipment <$> inv <*> equip,
        exploreWithAuto,
        descend <$> level <*> loc
        ]
      (moves, goText) = sendMoves move (R.whenE (fmap (/= MOUSE_MODE_TARGET) inputModeB) messages) (R.whenE stillAlive inputModeChanged)
                        (filterBy (\msg -> guard (msg ^? key "msg" == Just "menu") >> msg ^? key "tag"._String) demultiplexed)
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
