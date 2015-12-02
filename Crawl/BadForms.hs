{-# LANGUAGE OverloadedStrings #-}

module Crawl.BadForms where

import Data.List (find)

import qualified Data.HashMap.Strict as H

import Crawl.Move
import Crawl.Status

data Form = Normal | Tree | Fungus | Spider | Pig | Wisp

filterLegalInForm :: Player -> Maybe Move -> Maybe Move
filterLegalInForm p (Just (Go _ _))
  | hasStatus "Fear" p || hasStatus "Mesm" p = Just Rest
filterLegalInForm p (Just (Attack dx dy))
  | isConfused p = filterLegalInForm p (Just (Go dx dy))
filterLegalInForm p (Just m)
  | legalInForm p m = Just m
filterLegalInForm _ _ = Nothing

legalInForm :: Player -> Move -> Bool
legalInForm p m = case (form, m) of
  (_, ScanItem _ _) -> True
  (Spider, Wield _) -> False
  (Spider, Wear _) -> False
  (Spider, TakeOff _) -> False
  (Pig, Wield _) -> False
  (Pig, Wear _) -> False
  (Pig, TakeOff _) -> False
  (Wisp, Wield _) -> False
  (Wisp, Wear _) -> False
  (Wisp, TakeOff _) -> False
  (Tree, Attack _ _) -> True
  (Tree, Rest) -> True
  (Tree, LongRest) -> True
  (Tree, _) -> False
  (Fungus, Go _ _) -> False
  (Fungus, Wield _) -> False
  (Fungus, Wear _) -> False
  (Fungus, TakeOff _) -> False
  (_, _) -> True
  where form = maybe Normal snd $ find (\(light, _) -> light `H.member` _statuses p) badforms
        badforms = [("Tree", Tree), ("Fungus", Fungus), ("Spider", Spider), ("Pig", Pig), ("Wisp", Wisp)]
