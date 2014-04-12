{-# LANGUAGE OverloadedStrings #-}

module Crawl.Menu where

import qualified Data.Text as T
import qualified Data.Aeson as A

import Control.Lens ((^?!))
import Data.Aeson.Lens (key, _String)

data Menu = Menu {
  _menuTag :: T.Text,
  _menuTitle :: T.Text
  }

parseMenu :: A.Value -> Menu
parseMenu msg = Menu {
  _menuTag = msg ^?! key "tag"._String,
  _menuTitle = msg ^?! key "title".key "text"._String
  }
