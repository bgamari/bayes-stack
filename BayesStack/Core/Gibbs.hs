{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts,
             ExistentialQuantification, GADTs, CPP #-}

module BayesStack.Core.Gibbs ( UpdateUnit(..)
                             , WrappedUpdateUnit(..)
                             ) where

import Control.DeepSeq
import Data.Random

class (Show (Setting uu), Show uu) => UpdateUnit uu where
    type ModelState uu
    type Setting uu
    fetchSetting   :: uu -> ModelState uu -> Setting uu
    evolveSetting  :: ModelState uu -> uu -> RVar (Setting uu)
    updateSetting  :: uu -> Setting uu -> Setting uu -> ModelState uu -> ModelState uu

data WrappedUpdateUnit ms = forall uu. (UpdateUnit uu, ModelState uu ~ ms,
                                        NFData (Setting uu), Eq (Setting uu))
                         => WrappedUU uu

