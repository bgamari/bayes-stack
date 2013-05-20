module Data.Binary.EnumSet where

import Data.Binary
import Data.EnumSet

instance (Enum v, Binary v) => Binary (EnumSet v) where
  get = do a <- get
           return $ fromList a
  put = put . toList
