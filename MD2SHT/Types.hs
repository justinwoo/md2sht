module MD2SHT.Types where

import Data.Text

data Rule = Rule Selector [Line]
  deriving (Show, Eq, Ord)
newtype Selector = Selector Text
  deriving (Show, Eq, Ord)
data Line = Line Property Value
  deriving (Show, Eq, Ord)
newtype Property = Property Text
  deriving (Show, Eq, Ord)
newtype Value = Value Text
  deriving (Show, Eq, Ord)
