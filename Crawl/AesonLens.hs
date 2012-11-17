module Crawl.AesonLens (
  asObject,
  asArray,
  asString,
  asInteger,
  asDouble,
  asBool
  ) where

import Control.Applicative ((<$>), pure)

import Control.Lens (SimpleTraversal)
import qualified Data.Aeson as A
import qualified Data.Attoparsec.Number as A (Number(I, D))
import qualified Data.Text as T

asObject :: SimpleTraversal A.Value A.Object
asObject f (A.Object h) = A.Object <$> f h
asObject _ v = pure v

asArray :: SimpleTraversal A.Value A.Array
asArray f (A.Array a) = A.Array <$> f a
asArray _ v = pure v

asString :: SimpleTraversal A.Value T.Text
asString f (A.String t) = A.String <$> f t
asString _ v = pure v

asInteger :: SimpleTraversal A.Value Integer
asInteger f (A.Number (A.I n)) = A.Number . A.I <$> f n
asInteger _ v = pure v

asDouble :: SimpleTraversal A.Value Double
asDouble f (A.Number (A.D n)) = A.Number . A.D <$> f n
asDouble _ v = pure v

asBool :: SimpleTraversal A.Value Bool
asBool f (A.Bool b) = A.Bool <$> f b
asBool _ v = pure v

-- asNull??
