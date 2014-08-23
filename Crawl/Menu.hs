{-# LANGUAGE OverloadedStrings #-}

module Crawl.Menu where

import Control.Applicative ((<$>))
import Data.Char (ord, chr)
import Data.Maybe (fromMaybe)

import qualified Data.Text as T
import qualified Data.Aeson as A

import Control.Lens ((^?!), (^?), (^..), folding)
import Data.Aeson.Lens (key, _String, _Integer, values)

data MenuItem = MenuItem {
  _menuItemText :: !T.Text,
  _menuItemKey :: !Char
  }

data Menu = Menu {
  _menuTag :: T.Text,
  _menuTitle :: T.Text,
  _menuItems :: [MenuItem]
  }

parseMenuItem :: A.Value -> Maybe MenuItem
parseMenuItem item = MenuItem (item ^?! key "text"._String) <$> (item ^? key "hotkeys".values._Integer.folding itemKey)
  where itemKey n
          | i 'a' <= n && n <= i 'z' || i 'A' <= n && n <= i 'Z' = Just $ chr (fromInteger n)
          | otherwise = Nothing
        i = toInteger . ord

parseMenu :: A.Value -> Menu
parseMenu msg = Menu {
  _menuTag = msg ^?! key "tag"._String,
  _menuTitle = fromMaybe "" $ msg ^? key "title".key "text"._String,
  _menuItems = msg ^.. key "items".values.folding parseMenuItem
  }

anyItem :: Menu -> T.Text
anyItem Menu { _menuItems = MenuItem { _menuItemKey = k } : _ } = T.singleton k
anyItem _ = T.singleton '\ESC'
