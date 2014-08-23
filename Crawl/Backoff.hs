module Crawl.Backoff (
  backoff
  ) where

import Control.Applicative

import qualified Reactive.Banana as R

backoff :: (m -> Bool) -> R.Event t (m, Int) -> R.Behavior t Int -> R.Behavior t Bool
backoff mf failures time = backoffPolicy <$> time <*> past_failures
  where my_failures = fmap snd $ R.filterE (mf . fst) failures
        past_failures = R.accumB [] (fmap (:) my_failures)

-- Check if for any n, there are at least n elements of t that are >= t - 2^n
backoffPolicy :: Int -> [Int] -> Bool
backoffPolicy t ts = or $ zipWith (\n t' -> t' >= t - 2^n) [1::Int ..] ts
