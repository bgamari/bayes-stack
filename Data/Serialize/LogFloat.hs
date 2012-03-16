module Data.Serialize.LogFloat where

import Data.Serialize
import Data.Number.LogFloat

instance Serialize LogFloat where
  put = put . (logFromLogFloat :: LogFloat -> Double)
  get = (logToLogFloat :: Double -> LogFloat) `fmap` get

