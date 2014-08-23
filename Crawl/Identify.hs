{-# LANGUAGE ViewPatterns #-}

module Crawl.Identify (
  identify
  ) where

import Control.Monad (msum)

import qualified Data.Map as M

import Crawl.Inventory
import Crawl.Move
import Crawl.Status

identify :: Inventory -> Player -> Maybe Move
identify inv player = msum $ map (uncurry identifyItem) $ M.toList inv
  where identifyItem slot (itemType -> ItemPotion Nothing)
          | not (isBerserk player) = Just (Quaff slot)
        identifyItem _ _ = Nothing
