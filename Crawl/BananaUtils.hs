module Crawl.BananaUtils (
  filterBy,
  randomize
  ) where

import System.Random (mkStdGen, Random, randomR)

import qualified Reactive.Banana as R

filterBy :: (a -> Maybe b) -> R.Event t a -> R.Event t b
filterBy f = R.filterJust . fmap f

randomize :: (Random a) => (a, a) -> R.Event t b -> R.Event t a
randomize range clock = fst $ R.mapAccum (mkStdGen 17) (fmap (const $ randomR range) clock)
