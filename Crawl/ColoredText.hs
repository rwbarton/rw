{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module Crawl.ColoredText where

import Data.Monoid ((<>))

import qualified Data.Text as T

import Crawl.Bindings

type ColoredText = [(Color, T.Text)] -- Invariant: the individual texts should be nonempty

parseColoredText :: T.Text -> [(Color, T.Text)]
parseColoredText origText = filter (not . T.null . snd) $ go LIGHTGRAY "" origText -- Assume default color = lightgray
  where go currentColor prefix text = case T.breakOn "<" text of
          (start, maybeTag)
            | Just rest <- T.stripPrefix "<<" maybeTag
              -> go currentColor (prefix <> start <> "<") rest
            | T.null maybeTag -> [(currentColor, prefix <> start)]
            | Just tagBody <- T.stripPrefix "<" maybeTag,
              (colorName, tagEnd) <- T.breakOn ">" tagBody,
              Just rest <- T.stripPrefix ">" tagEnd
              -> (currentColor, prefix <> start) :
                 go (readColor colorName) "" rest
            | otherwise -> error $ "could not parseColoredText: " ++ T.unpack origText

readColor :: T.Text -> Color
readColor "black" = BLACK
readColor "blue" = BLUE
readColor "green" = GREEN
readColor "cyan" = CYAN
readColor "red" = RED
readColor "magenta" = MAGENTA
readColor "brown" = BROWN
readColor "lightgrey" = LIGHTGRAY -- ...
readColor "darkgrey" = DARKGRAY   -- ...
readColor "lightblue" = LIGHTBLUE
readColor "lightgreen" = LIGHTGREEN
readColor "lightcyan" = LIGHTCYAN
readColor "lightred" = LIGHTRED
readColor "lightmagenta" = LIGHTMAGENTA
readColor "yellow" = YELLOW
readColor "white" = WHITE
readColor col = error $ "unexpected color " ++ T.unpack col
