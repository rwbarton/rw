module Crawl.BananaUtils (
  filterBy
  ) where

import qualified Reactive.Banana as R

filterBy :: (a -> Maybe b) -> R.Event t a -> R.Event t b
filterBy f = R.filterJust . fmap f
