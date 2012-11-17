module Crawl.BananaUtils (
  filterBy,
  actualChanges
  ) where

import qualified Reactive.Banana as R

filterBy :: (a -> Maybe b) -> R.Event t a -> R.Event t b
filterBy f = R.filterJust . fmap f

actualChanges :: Eq a => R.Behavior t a -> R.Event t b -> R.Event t a
actualChanges beh clock = R.filterApply (fmap (\b c -> b /= Just c) beh') clock'
  where clock' = R.apply (fmap const beh) clock
        beh' = R.stepper Nothing (fmap Just clock')
