{-# LANGUAGE DeriveGeneric #-}

module BayesStack.Dirichlet ( -- * Dirichlet parameter
                              Dirichlet
                            , symAlpha, asymAlpha
                            , alphaDomain, alphaNormalizer, sumAlpha
                            , DirMean, DirPrecision
                            , alphaOf, setAlphaOf, setSymAlpha
                            , alphaToMeanPrecision, meanPrecisionToAlpha
                            , symmetrizeAlpha
                            , prettyAlpha
                            ) where

import Data.Foldable (toList, Foldable, fold)

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM

import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

import Numeric.Log
import Math.Gamma

import Text.Printf
import Text.PrettyPrint

import Data.Binary
import Data.Binary.EnumMap ()
import GHC.Generics (Generic)

-- | Make error handling a bit easier
checkNaN :: RealFloat a => String -> a -> a
checkNaN loc x | isNaN x = error $ "BayesStack.Dirichlet."++loc++": Not a number"
checkNaN loc x | isInfinite x = error $ "BayesStack.Dirichlet."++loc++": Infinity"
checkNaN _ x = x

-- | A Dirichlet distribution
data Dirichlet a
    -- | Symmetric
    = Sym { aDomain :: Seq a
        , aAlpha :: !Double
        , aNorm :: Log Double
        }
    -- | Asymmetric
    | Asym { aAlphas :: EnumMap a Double
           , aSumAlphas :: !Double
           , aNorm :: Log Double
           }
    deriving (Show, Eq, Generic)
instance (Enum a, Binary a) => Binary (Dirichlet a)

type DirMean a = EnumMap a Double
type DirPrecision = Double

symAlpha :: Enum a => [a] -> Double -> Dirichlet a
symAlpha domain _ | null domain = error "Dirichlet over null domain is undefined"
symAlpha domain alpha = Sym { aDomain = SQ.fromList domain
                            , aAlpha = alpha
                            , aNorm = alphaNorm $ symAlpha domain alpha
                            }

-- | Construct an asymmetric Alpha
asymAlpha :: Enum a => EnumMap a Double -> Dirichlet a
asymAlpha alphas | EM.null alphas = error "Dirichlet over null domain is undefined"
asymAlpha alphas =
    Asym { aAlphas = alphas
         , aSumAlphas = Prelude.sum $ EM.elems alphas
         , aNorm = alphaNorm $ asymAlpha alphas
         }

setSymAlpha :: Enum a => Double -> Dirichlet a -> Dirichlet a
setSymAlpha alpha a =
    let b = (symmetrizeAlpha a) { aAlpha = alpha
                                , aNorm = alphaNorm b
                                }
    in b

-- | Compute the normalizer of the likelihood involving alphas,
-- (product_k gamma(alpha_k)) / gamma(sum_k alpha_k)
alphaNorm :: Enum a => Dirichlet a -> Log Double
alphaNorm alpha = normNum / normDenom
  where dim = realToFrac $ SQ.length $ aDomain alpha
        normNum = case alpha of
                      Asym {} -> product $ map (\a->Exp $ checkNaN ("alphaNorm.normNum(asym) alpha="++show a) $ lnGamma a)
                                  $ EM.elems $ aAlphas alpha
                      Sym {}  -> Exp $ checkNaN "alphaNorm.normNum(sym)" $ dim * lnGamma (aAlpha alpha)
        normDenom = Exp $ checkNaN "alphaNorm.normDenom" $ lnGamma $ sumAlpha alpha

-- | 'alphaDomain a' is the domain of prior 'a'
alphaDomain :: Enum a => Dirichlet a -> Seq a
alphaDomain (Sym {aDomain=d}) = d
alphaDomain (Asym {aAlphas=a}) = SQ.fromList $ EM.keys a
{-# INLINE alphaDomain #-}

alphaNormalizer :: Enum a => Dirichlet a -> Log Double
alphaNormalizer = aNorm
{-# INLINE alphaNormalizer #-}

-- | 'alphaOf alpha k' is the value of element 'k' in prior 'alpha'
alphaOf :: Enum a => Dirichlet a -> a -> Double
alphaOf (Sym {aAlpha=alpha}) = const alpha
alphaOf (Asym {aAlphas=alphas}) = (alphas EM.!)
{-# INLINE alphaOf #-}

-- | 'sumAlpha alpha' is the sum of all alphas
sumAlpha :: Enum a => Dirichlet a -> Double
sumAlpha (Sym {aDomain=domain, aAlpha=alpha}) = realToFrac (SQ.length domain) * alpha
sumAlpha (Asym {aSumAlphas=sum}) = sum
{-# INLINE sumAlpha #-}

-- | Set a particular alpha element
setAlphaOf :: Enum a => a -> Double -> Dirichlet a -> Dirichlet a
setAlphaOf k a alpha@(Sym {}) = setAlphaOf k a $ asymmetrizeAlpha alpha
setAlphaOf k a (Asym {aAlphas=alphas}) = asymAlpha $ EM.insert k a alphas
{-# INLINE setAlphaOf #-}

-- | 'alphaToMeanPrecision a' is the mean/precision representation of the prior 'a'
alphaToMeanPrecision :: Enum a => Dirichlet a -> (DirMean a, DirPrecision)
alphaToMeanPrecision (Sym {aDomain=dom, aAlpha=alpha}) =
  let prec = realToFrac (SQ.length dom) * alpha
  in (EM.fromList $ map (\a->(a, alpha/prec)) $ toList dom, prec)
alphaToMeanPrecision (Asym {aAlphas=alphas, aSumAlphas=prec}) =
  (fmap (/prec) alphas, prec)
{-# INLINE alphaToMeanPrecision #-}

-- | 'meanPrecisionToAlpha m p' is a prior with mean 'm' and precision 'p'
meanPrecisionToAlpha :: Enum a => DirMean a -> DirPrecision -> Dirichlet a
meanPrecisionToAlpha mean prec = asymAlpha $ fmap (*prec) mean
{-# INLINE meanPrecisionToAlpha #-}

-- | Symmetrize a Dirichlet prior (such that mean=0)
symmetrizeAlpha :: Enum a => Dirichlet a -> Dirichlet a
symmetrizeAlpha alpha@(Sym {}) = alpha
symmetrizeAlpha alpha@(Asym {}) =
  Sym { aDomain = alphaDomain alpha
           , aAlpha = sumAlpha alpha / realToFrac (EM.size $ aAlphas alpha)
           , aNorm = alphaNorm $ symmetrizeAlpha alpha
           }

-- | Turn a symmetric alpha into an asymmetric alpha. For internal use.
asymmetrizeAlpha :: Enum a => Dirichlet a -> Dirichlet a
asymmetrizeAlpha (Sym {aDomain=domain, aAlpha=alpha}) =
  asymAlpha $ fold $ fmap (\k->EM.singleton k alpha) domain
asymmetrizeAlpha alpha@(Asym {}) = alpha

-- | Pretty-print a Dirichlet prior
prettyAlpha :: Enum a => (a -> String) -> Dirichlet a -> Doc
prettyAlpha showA (Sym {aAlpha=alpha}) = text "Symmetric" <+> double alpha
prettyAlpha showA (Asym {aAlphas=alphas}) =
  text "Assymmetric"
  <+> fsep (punctuate comma
           $ map (\(a,alpha)->text (showA a) <> parens (text $ printf "%1.2e" alpha))
           $ take 100 $ EM.toList $ alphas)
