module Data.Binary.LogFloat where

import Data.Binary
import Data.Number.LogFloat

instance Binary LogFloat where
  put = put . (logFromLogFloat :: LogFloat -> Double)
  get = (logToLogFloat :: Double -> LogFloat) `fmap` get
