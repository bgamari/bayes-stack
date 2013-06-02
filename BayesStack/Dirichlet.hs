{-# LANGUAGE DeriveGeneric #-}

module BayesStack.Dirichlet ( -- * Dirichlet parameter
                              Dirichlet
                              -- * Creation
                            , fromPrecision
                            , fromConcentrations
                            , fromMeanPrecision
                              -- * Querying
                            , domain, dimension, normalizer
                            , alphaOf, pmf
                            , mean, precision
                            , isSymmetric
                            , symmetrize
                              -- * Utilities
                            , prettyPrint
                              -- * Precision
                            , Precision(..)
                            , precisionToDir
                            ) where

import Data.Foldable (toList, Foldable, fold, foldl', foldMap)
import Data.Monoid

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM

import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as ES

import Numeric.Log
import Math.Gamma

import Text.Printf
import Text.PrettyPrint hiding ((<>))

import Data.Binary
import Data.Binary.EnumMap ()
import Data.Binary.EnumSet ()
import GHC.Generics (Generic)

-- | Make error handling a bit easier
checkNaN :: RealFloat a => String -> a -> a
checkNaN loc x | isNaN x = error $ "BayesStack.Dirichlet."++loc++": Not a number"
checkNaN loc x | isInfinite x = error $ "BayesStack.Dirichlet."++loc++": Infinity"
checkNaN _ x = x

-- | A Dirichlet distribution
data Dirichlet a
    = Sym { dimension  :: !Int                -- ^ Dimension of domain
          , _domain    :: !(EnumSet a)        -- ^ Elements of domain
          , conc       :: !Double             -- ^ Concentration parameter
          , precision  :: !Double             -- ^ Sum of alphas
          , norm       :: Log Double          -- ^ Normalizer
          }
    | Asym { dimension :: !Int
           , concs     :: !(EnumMap a Double) -- ^ Elements of domain
           , precision :: !Double             -- ^ Elements of domain
           , norm      :: Log Double          -- ^ Normalizer
           }
    deriving (Show, Eq, Generic)
instance (Enum a, Binary a) => Binary (Dirichlet a)

domain :: Enum a => Dirichlet a -> EnumSet a
domain (Sym {_domain=d}) = d
domain (Asym {concs=c}) = EM.keysSet c

-- | Construct a symmetric Dirichlet distribution from the dimension of domain.
fromPrecision :: Enum a => Double -> [a] -> Dirichlet a
fromPrecision alpha domain =
  let a = Sym n dom alpha (alpha * fromIntegral n) (computeNorm a)
      dom = ES.fromList domain
      n = ES.size dom
  in a
{-# INLINE fromPrecision #-}

-- | Construct an asymmetric Dirichlet from the size of the distribution
fromConcentrations :: Enum a => [(a,Double)] -> Dirichlet a
fromConcentrations alphas
  | null alphas = error "Dirichlet over null domain is undefined"
  | otherwise = a
    where go (n,alphas,prec) (a,w) = (n+1, EM.insert a w alphas, prec+w)
          (n, alphas', prec) = foldl' go (0, EM.empty, 0) alphas
          a = Asym n alphas' prec (computeNorm a)

-- | Construct an asymmetric Dirichlet from a mean and precision
fromMeanPrecision :: Enum a => [(a, Double)] -> Double -> Dirichlet a
fromMeanPrecision mean prec = fromConcentrations $ map (fmap (*prec)) mean
{-# INLINE fromMeanPrecision #-}

-- | Compute the normalizer of the likelihood involving alphas,
-- (product_k gamma(alpha_k)) / gamma(sum_k alpha_k)
-- (used internally)
computeNorm :: Dirichlet a -> Log Double
computeNorm d = normNum / normDenom
  where dim = realToFrac $ dimension d
        normNum = case d of
                    Asym {concs=alphas} ->
                      product $ map (\a->Exp $ checkNaN ("computeNorm alpha="++show a)
                                             $ lnGamma a)
                              $ EM.elems $ alphas
                    Sym {conc=alpha}    ->
                      Exp $ checkNaN "computeNorm"
                          $ dim * lnGamma alpha
        normDenom = Exp $ checkNaN "alphaNorm.normDenom"
                        $ lnGamma $ precision d

-- | Probability of the given element being drawn from the given distribution  
pmf :: Enum a => Foldable f => Dirichlet a -> f (a, Double) -> Log Double
pmf d xs = 1/norm d * getProduct (foldMap (\(x,p)->Product $ (realToFrac p)**(realToFrac $ alphaOf d x - 1)) xs)

normalizer :: Enum a => Dirichlet a -> Log Double
normalizer = norm
{-# INLINE normalizer #-}

-- | 'alphaOf alpha k' is the value of element 'k' in prior 'alpha'
alphaOf :: Enum a => Dirichlet a -> a -> Double
alphaOf (Sym {conc=a}) x = a
alphaOf (Asym {concs=alphas}) x = alphas EM.! x
{-# INLINE alphaOf #-}

-- | Is a Dirichlet symmetric?
isSymmetric :: Dirichlet a -> Bool
isSymmetric (Sym {}) = True
isSymmetric _        = False

{-            
-- | Set a particular alpha element
setAlphaOf :: Enum a => a -> Double -> Dirichlet a -> Dirichlet a
setAlphaOf k a alpha@(Dir {conc=Sym _})  = setAlphaOf k a $ asymmetrizeAlpha alpha
setAlphaOf k a (Asym {conc=Asym alphas}) = asymAlpha $ EM.insert k a alphas
{-# INLINE setAlphaOf #-}
-}    

-- | The mean of a Dirichlet. This isn't possible in the symmetric
-- case as we don't know the domain.
mean :: Enum a => Dirichlet a -> Maybe [(a, Double)]
mean (Sym {})                         = Nothing
mean (Asym {concs=a, precision=prec}) = Just $ EM.toList $ fmap (/ prec) a

-- | Symmetrize a Dirichlet distribution such that mean=0 yet preserving precision
symmetrize :: Enum a => Dirichlet a -> Dirichlet a
symmetrize alpha@(Sym {}) = alpha
symmetrize alpha =
    let a = alpha { conc = precision alpha / realToFrac (dimension alpha)
                  , norm = computeNorm a
                  }
    in a

asymmetrize :: Enum a => Dirichlet a -> Dirichlet a
asymmetrize alpha@(Asym {}) = alpha            
asymmetrize alpha@(Sym {conc=a}) =
    Asym { dimension = dimension alpha
         , concs     = foldMap (\x->EM.singleton x a) $ ES.toList $ domain alpha
         , precision = precision alpha
         , norm      = norm alpha
         }

-- | Pretty-print a Dirichlet prior
prettyPrint :: Enum a => (a -> String) -> Dirichlet a -> Doc
prettyPrint showA (Sym {conc=alpha}) = text "Symmetric" <+> double alpha
prettyPrint showA (Asym {concs=alphas}) =
  text "Assymmetric"
  <+> fsep (punctuate comma
           $ map (\(a,alpha)->text (showA a) <> parens (text $ printf "%1.2e" alpha))
           $ take 100 $ EM.toList $ alphas)

-- | This encodes a Dirichlet with unknown domain but known precision
newtype Precision a = Precision Double
                    deriving (Show, Eq, Generic)
instance Binary (Precision a)
         
-- | Precision
precisionToDir :: Enum a => Precision a -> [a] -> Dirichlet a
precisionToDir (Precision p) = fromPrecision p