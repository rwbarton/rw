{-# LANGUAGE OverloadedStrings #-}

module Crawl.BadForms where

import qualified Data.HashMap.Strict as H

import Crawl.Move
import Crawl.Status

data Form = Normal | Tree

filterLegalInForm :: Player -> Maybe Move -> Maybe Move
filterLegalInForm p (Just m)
  | legalInForm p m = Just m
filterLegalInForm _ _ = Nothing

legalInForm :: Player -> Move -> Bool
legalInForm p m = case (form, m) of
  (_, ScanItem _ _) -> True
  (Tree, Attack _ _) -> True
  (Tree, Rest) -> True
  (Tree, LongRest) -> True
  (Tree, _) -> False
  (_, _) -> True
  where form = if "Tree" `H.member` _statuses p
               then Tree
               else Normal
