module Data.Binary.EnumMap where

import Data.Binary
import Data.EnumMap

instance (Enum k, Binary k, Binary v) => Binary (EnumMap k v) where
  get = do a <- get
           return $ fromList a
  put = put . toList
