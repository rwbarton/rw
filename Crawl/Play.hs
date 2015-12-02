{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Crawl.Play (
  play
  ) where

import Control.Applicative ((<$>), pure, (<*>), liftA2, (<|>))
import Control.Monad (forever, guard)
import Data.List (intersect)
import Data.Maybe (fromMaybe, listToMaybe, isJust)
import System.Exit (exitWith, ExitCode(ExitSuccess))

import Control.Concurrent.Chan.Split (InChan, OutChan, writeChan, readChan)
import Control.Lens ((^..), (^?), at, to, traverse, enum)
import Data.Aeson.Lens (_Integer, _String, _Array, key)
import Numeric.Lens (integral)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Reactive.Banana as R
import qualified Reactive.Banana.Frameworks as R
import qualified Data.Text as T

import Crawl.Backoff
import Crawl.BadForms
import Crawl.BananaUtils
import Crawl.Bindings
import Crawl.Equipment
import Crawl.Explore
import Crawl.FloorItems
import Crawl.Identify
import Crawl.Inventory
import Crawl.Item
import Crawl.LevelInfo
import Crawl.Messages
import Crawl.Move
import Crawl.Menu
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

      needRest = (\p -> _hp p < _mhp p || isSlow p) <$> player
      rest = (\nr lm t -> if nr
                          then case lm of
                            (t', LongRest) | t' == t -> Just Rest
                            _ -> Just LongRest
                          else Nothing) <$> needRest <*> lastMove <*> (_time <$> player)

      eatChunk =
        (\p i -> listToMaybe $ do
            (slot, item@(itemData -> ItemFood FOOD_CHUNK)) <- M.toList i
            guard $ not (itemColor item `elem` [LIGHTGREEN, LIGHTRED, DARKGRAY])
            guard $ hungerLevel p < HS_SATIATED -- for normal races
            return $ Eat slot) <$> player <*> inv
      eatPermafood =
        (\i -> listToMaybe $ do
            (slot, (itemData -> ItemFood foodType)) <- M.toList i
            guard $ foodType /= FOOD_CHUNK
            return $ Eat slot) <$> inv
      eatAnything = (<|>) <$> eatChunk <*> eatPermafood
      -- Always eat a chunk if possible; permafood only if very hungry
      eat = (\p ec ep -> ec <|> (guard (hungerLevel p < HS_HUNGRY) >> ep))
            <$> player <*> eatChunk <*> eatPermafood
      eatWhenStarving = (\p ea -> guard (hungerLevel p <= HS_STARVING) >> ea)
                        <$> player <*> eatAnything

      floorItems = trackFloorItems cursor level inputModeB messages lastMove loc moves inputModeChanged

      pickupRune =
        (\l i -> do
            (_, is) <- H.lookup l i
            guard $ any isRune . knownItems $ is
            return (PickUp isRune)) <$> loc <*> floorItems

      pickup =
        (\l i wi -> do
            (_, is) <- H.lookup l i
            guard $ any wi . knownItems $ is
            return (PickUp wi)) <$> loc <*> floorItems <*> (fmap (wantItem False) inv)

      corpses = fmap (HS.fromList . H.keys . H.filter (any butcherable . knownItems . snd)) floorItems
      useCorpse = (\l c ->
                    guard (HS.member l c) >>
                    return Butcher) <$> loc <*> corpses
      -- 'loot' is responsible for getting us to the corpse

      burnBooks =
        (\fi l p -> do
            guard (any isBook $ concatMap (knownItems . snd . snd) . filter ((/= l) . fst) $ H.toList fi)
            guard $ canUseGodAbility "Trog" 0 p
            return BurnBooks) <$> floorItems <*> loc <*> player

      threatened =
        (\ll l ->
            let monstersInView = [ (monType, dist sq l)
                                 | sq <- HS.toList $ _levelLOS ll,
                                   Just (_monsterType -> monType) <- return (H.lookup sq (_levelMonsters ll)),
                                   not $ nonthreatening monType ]
            in not . null $ monstersInView) <$> level <*> loc
      nonthreatening m = m `elem` [MONS_TOADSTOOL, MONS_FUNGUS, MONS_PLANT, MONS_BUSH,
                                   MONS_BALLISTOMYCETE, MONS_HYPERACTIVE_BALLISTOMYCETE]
      dist (Coord x1 y1) (Coord x2 y2) = max (abs (x1 - x2)) (abs (y1 - y2))

      cureConfusion =
        (\t p i -> do
            guard (t && isConfused p && not (isBerserk p))
            slot <- listToMaybe [ slot | (slot, itemData -> ItemPotion (Just POT_CURING)) <- M.toList i ]
            return $ Quaff slot) <$> threatened <*> player <*> inv

      healWounds =
        (\t p i -> do
            guard (t && (2 * _hp p < _mhp p) && not (isBerserk p))
            slot <- listToMaybe [ slot | (slot, itemData -> ItemPotion (Just POT_HEAL_WOUNDS)) <- M.toList i ]
            return $ Quaff slot) <$> threatened <*> player <*> inv

      berserk =
        (\p i ll l -> do
            let teleInstead =
                  guard (not (canTrogBerserk p) && not (hasStatus "Tele" p) && canRead p && lowHP p) >>
                  listToMaybe [ slot | (slot, itemData -> ItemScroll (Just SCR_TELEPORTATION)) <- M.toList i ]
            guard $ canTrogBerserk p || isJust teleInstead
            let monstersInView = [ (monType, dist sq l)
                                 | sq <- HS.toList $ _levelLOS ll,
                                   Just (_monsterType -> monType) <- return (H.lookup sq (_levelMonsters ll)),
                                   not $ nonthreatening monType ]
            guard $ not . null $ monstersInView
            guard $ lowHP p && not (null $ filter ((<= 1) . snd) monstersInView)
              || sum [ meleeThreat m | (m, d) <- monstersInView, d <= 2] >= _xl p
            return $ maybe Berserk Read teleInstead) <$> player <*> inv <*> level <*> loc
        where meleeThreat MONS_GNOLL = 1
              meleeThreat MONS_WORM = 1
              meleeThreat MONS_IGUANA = 2
              meleeThreat MONS_ORC_WIZARD = 3
              meleeThreat MONS_ORC_PRIEST = 3
              meleeThreat MONS_IJYB = 3
              meleeThreat MONS_TERENCE = 3
              meleeThreat MONS_ICE_BEAST = 4
              meleeThreat MONS_CRAZY_YIUF = 4
              meleeThreat MONS_OGRE = 6
              meleeThreat MONS_ORC_WARRIOR = 6
              meleeThreat MONS_SKELETAL_WARRIOR = 7
              meleeThreat MONS_PURGY = 8
              meleeThreat MONS_SIGMUND = 8
              meleeThreat MONS_HILL_GIANT = 10
              meleeThreat MONS_ORC_KNIGHT = 12
              meleeThreat MONS_URUG = 12
              meleeThreat MONS_ERICA = 14
              meleeThreat MONS_ORC_WARLORD = 16
              meleeThreat MONS_HYDRA = 16
              meleeThreat MONS_ETTIN = 16
              meleeThreat MONS_RUPERT = 18
              meleeThreat MONS_SNORG = 18
              meleeThreat MONS_PLAYER_GHOST = 27
              meleeThreat _ = 0

              lowHP p = 2 * _hp p < _mhp p

      trogsHand =
        (\p l -> do
            guard (not (hasStatus "Regen MR++" p) && canTrogsHand p)
            let monstersInView = [ _monsterType mon | sq <- HS.toList $ _levelLOS l, Just mon <- return (H.lookup sq (_levelMonsters l)) ]
            guard $ isPoisoned p && _hp p <= 2 || not (null $ monstersInView `intersect` trogsHandMonsters)
            return TrogsHand) <$> player <*> level
        where trogsHandMonsters = -- http://www.reddit.com/r/roguelikes/comments/1eq3g6/dcss_advice_for_mibe/ca2pwni
                [MONS_WIZARD, MONS_OGRE_MAGE, MONS_DEEP_ELF_MAGE, MONS_EROLCHA, MONS_DEEP_ELF_SORCERER,
                 MONS_DEEP_ELF_DEMONOLOGIST, MONS_ORC_SORCERER, MONS_SPHINX, MONS_GREAT_ORB_OF_EYES, MONS_GOLDEN_EYE,
                 MONS_LICH, MONS_ANCIENT_LICH, MONS_RUPERT, MONS_NORRIS, MONS_AIZUL, MONS_MENNAS, MONS_LOUISE,
                 MONS_JORGRUN, MONS_DRACONIAN_SHIFTER, MONS_CACODEMON, MONS_PANDEMONIUM_LORD, MONS_ERESHKIGAL] ++
                [MONS_SIGMUND, MONS_GRINDER]

      blinkToRune =
        (\p i fi pCoord@(Coord px py) -> do
            guard (canRead p)
            blinkSlot <- listToMaybe [ slot | (slot, itemData -> ItemScroll (Just SCR_BLINKING)) <- M.toList i ]
            Coord rx ry <- listToMaybe [ runeCoord | (runeCoord, (_, items)) <- H.toList fi, runeCoord /= pCoord, item <- knownItems items, isRune item ]
            return (BlinkTo blinkSlot (rx-px) (ry-py))) <$> player <*> inv <*> floorItems <*> loc

      bia =
        (\p l -> do
            guard (canBiA p)
            let monstersInView = [ _monsterType mon | sq <- HS.toList $ _levelLOS l, Just mon <- return (H.lookup sq (_levelMonsters l)) ]
            let alliesInView = [ _monsterType mon | sq <- HS.toList $ _levelLOS l, Just mon <- return (H.lookup sq (_levelMonsters l)), _monsterAttitude mon == ATT_FRIENDLY ]
            guard $ null alliesInView
            guard $ not (null $ monstersInView `intersect` biaMonsters)
            return BiA) <$> player <*> level
        where biaMonsters = [MONS_HYDRA, MONS_OKLOB_PLANT, MONS_SONJA]

      moveFailures = filterBy (\(t, (t', lm), im) -> guard (im == MOUSE_MODE_COMMAND && t == t') >> return (lm,t)) $
                     (,,) <$> (_time <$> player) <*> lastMove R.<@> inputModeEvents

      exploreWithAuto =
        (\ll l b -> (if b then id else const AutoExplore) <$> explore ll l)
        <$> level <*> loc <*> backoff isAutoExplore moveFailures (_time <$> player)
        where isAutoExplore AutoExplore = True
              isAutoExplore _ = False

      killWithTab =
        (\ll l p lm t ->
          case kill ll l p of
            Nothing -> Nothing
            Just m -> case (m, lm) of
              (_, (t', AutoFight)) | t' == t -> Just m
              (Attack _ _, _) -> Just AutoFight
              _ -> Just m) <$> level <*> loc <*> player <*> lastMove <*> (_time <$> player)

      killWithTabIfThreatened =
        (\t k -> if t then k else Nothing) <$> threatened <*> killWithTab

      invisibleMonsters =
        R.stepper False $
        (const True <$> R.filterE ((== "<lightred>Deactivating autopickup; reactivate with <white>Ctrl-A<lightred>.<lightgrey>") . _msgText) messages)
        `R.union` (const False <$> R.filterE (isReactivatingAutopickup . _msgText) messages)
        where isReactivatingAutopickup "<lightred>Reactivating autopickup.<lightgrey>" = True
              isReactivatingAutopickup "<lightgrey>Autopickup is now on.<lightgrey>" = True
              isReactivatingAutopickup _ = False
      killInvisible = (\ims u rnd -> guard ims >> if rnd /= (0 :: Int) then Just (Attack (dxs !! u) (dys !! u)) else Just AutoPickup) <$> invisibleMonsters <*> R.stepper 0 (randomize (0, 7) demultiplexed) <*> R.stepper 0 (randomize (0, 26) demultiplexed)
        where dxs = [-1, -1, -1, 0, 1, 1, 1, 0]
              dys = [-1, 0, 1, 1, 1, 0, -1, -1]

      useGoodConsumables i =
        listToMaybe $
        [ Quaff slot | (slot, itemData -> ItemPotion (Just s)) <- M.toList i, s `elem` [POT_EXPERIENCE, POT_BENEFICIAL_MUTATION] ] ++
        [ Read slot | (slot, itemData -> ItemScroll (Just SCR_ACQUIREMENT)) <- M.toList i ]

      fly = (\p -> guard (_species p == "Gargoyle" && _xl p >= 14 && _mp p >= 3 && not (hasStatus "Fly" p)) >> return GargoyleFlight) <$> player

      beenTo = fmap (flip HS.member) $
        R.accumB HS.empty $
        (HS.insert . dlvl <$> player) R.<@ moves

      lastDump = R.stepper 0 $ (_time <$> player) R.<@ R.filterE (\mv -> case mv of { Dump -> True; _ -> False }) moves
      dump = (\l t -> guard (t `div` 10000 > l `div` 10000) >> Just Dump) <$> lastDump <*> (_time <$> player)


      move = foldr (liftA2 (flip fromMaybe)) (pure Rest) $ map (fmap filterLegalInForm player <*>) [
        dump,
        scanFloorItems <$> level <*> loc <*> floorItems,
        eatWhenStarving,
        cureConfusion,
        blinkToRune,
        pickupRune,
        bia,
        berserk,
        healWounds,
        trogsHand,
        enterBranches <$> level <*> loc <*> beenTo,
        killInvisible,
        killWithTabIfThreatened,
        eat,
        useCorpse,
        burnBooks,
        pickup,
        rest,
        killWithTab,
        loot <$> level <*> loc <*> floorItems <*> inv, -- should probably produce set of things we want here, not in Explore
        upgradeEquipment <$> inv <*> equip <*> player <*> eatAnything,
        dropJunkEquipment <$> inv <*> equip,
        enchantEquipment <$> inv <*> equip,
        useGoodConsumables <$> inv,
        fly,
        exploreWithAuto,
        identify <$> inv <*> player,
        descend <$> level <*> loc <*> (dlvl <$> player) <*> beenTo
        ]
      (moves, goText) = sendMoves move messages (R.whenE stillAlive inputModeChanged)
                        (filterBy (\msg -> guard (msg ^? key "msg" == Just "menu") >> return (parseMenu msg)) demultiplexed)
      lastMove = R.stepper (0, GoDown) {- whatever -} $ (,) <$> (_time <$> player) R.<@> moves
      clearText = fmap (const " ") goodbye

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
