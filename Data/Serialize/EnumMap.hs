module Data.Serialize.EnumMap where

import Data.Serialize
import Data.EnumMap

instance (Enum k, Serialize k, Serialize v) => Serialize (EnumMap k v) where
  get = do a <- get
           return $ fromList a
  put = put . toList

