{-# LANGUAGE DeriveGeneric #-}

module BayesStack.Dirichlet ( -- * Dirichlet parameter
                              Dirichlet
                              -- * Creation
                            , fromDim, fromDomain
                            , fromConcentrations
                            , fromMeanPrecision
                              -- * Querying
                            , dimension, normalizer
                            , alphaOf, pmf
                            , mean, precision
                            , isSymmetric
                            , symmetrize
                              -- Utilities
                            , prettyPrint
                            ) where

import Data.Foldable (toList, Foldable, fold, foldl')

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EM

import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as ES

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

data Concentration a = Sym !Double
                     | Asym !(EnumMap a Double)
    deriving (Show, Eq, Generic)
instance (Enum a, Binary a) => Binary (Concentration a)

-- | A Dirichlet distribution
data Dirichlet a
    = Dir { dimension  :: !Int                -- ^ Dimension of domain
          , conc       :: !(Concentration a)  -- ^ Concentration parameter
          , precision  :: !Double             -- ^ Sum of alphas
          , norm       :: Log Double          -- ^ Normalizer
          }
    deriving (Show, Eq, Generic)
instance (Enum a, Binary a) => Binary (Dirichlet a)

-- | Construct a symmetric Dirichlet distribution from the dimension of domain.
fromDim :: Int -> Double -> Dirichlet a
fromDim n alpha
  | n < 0    = error "BayesStack.Dirichlet.symAlpha: Negative dimension"
  | otherwise = let a = Dir n (Sym alpha) (alpha * fromIntegral n) (computeNorm a)
                in a

-- | Contruct a symmetric Dirichlet distribution from a domain
fromDomain :: [a] -> Double -> Dirichlet a
fromDomain xs a = fromDim (length xs) a
{-# INLINE fromDomain #-}

-- | Construct an asymmetric Dirichlet from the size of the distribution
fromConcentrations :: Enum a => [(a,Double)] -> Dirichlet a
fromConcentrations alphas
  | null alphas = error "Dirichlet over null domain is undefined"
  | otherwise = a
    where go (n,alphas,prec) (a,w) = (n+1, EM.insert a w alphas, prec+w)
          (n, alphas', prec) = foldl' go (0, EM.empty, 0) alphas
          a = Dir n (Asym alphas') prec (norm a)


-- | Construct an asymmetric Dirichlet from a mean and precision
fromMeanPrecision :: Enum a => [(a, Double)] -> Double -> Dirichlet a
fromMeanPrecision mean prec = fromConcentrations $ map (fmap (*prec)) mean
                  
{-# INLINE fromMeanPrecision #-}
-- | Combine Dirichlets with counts @a_i@ and @b_i@ such that @c_i = a_i + b_i@
-- This is only possible in cases where the domains are equivalent
add :: Dirichlet a -> Dirichlet a -> Maybe (Dirichlet a)
add a b | dimension a /= dimension b  = Nothing 
add a b = 
    let prec = precision a + precision b
        fromAlphas alphas = let c = Dir (dimension a) alphas prec (computeNorm c)
                            in Just c
    in case (conc a, conc b) of
         (Sym x, Sym y)     -> fromAlphas (Sym $ x + y)
         (Sym x, Asym ys)   -> fromAlphas (Asym $ fmap (+x) ys)
         (Asym xs, Asym ys) -> fromAlphas (Asym $ EM.unionWith (+) xs ys)
         (Asym ys, Sym x)   -> add b a

-- | Compute the normalizer of the likelihood involving alphas,
-- (product_k gamma(alpha_k)) / gamma(sum_k alpha_k)
-- (used internally)
computeNorm :: Dirichlet a -> Log Double
computeNorm alpha = normNum / normDenom
  where dim = realToFrac $ dimension alpha
        normNum = case conc alpha of
                    Asym alphas -> product
                                   $ map (\a->Exp $ checkNaN ("computeNorm alpha="++show a)
                                                  $ lnGamma a)
                                   $ EM.elems $ alphas
                    Sym alpha   -> Exp $ checkNaN "computeNorm"
                                       $ dim * lnGamma alpha
        normDenom = Exp $ checkNaN "alphaNorm.normDenom"
                        $ lnGamma $ precision alpha

-- | Probability of the given element being drawn from the given distribution  
pmf :: Foldable f => Dirichlet a -> f a -> Log Double
pmf d xs = 1/norm d * getProduct $ foldMap (\x->Product $ x**(alphaOf d x - 1)) xs

normalizer :: Enum a => Dirichlet a -> Log Double
normalizer = norm
{-# INLINE normalizer #-}

-- | 'alphaOf alpha k' is the value of element 'k' in prior 'alpha'
alphaOf :: Enum a => Dirichlet a -> a -> Double
alphaOf (Dir {conc=Sym a}) = const a
alphaOf (Dir {conc=Asym alphas}) = (alphas EM.!)
{-# INLINE alphaOf #-}

-- | Is a Dirichlet symmetric?
isSymmetric :: Dirichlet a -> Bool
isSymmetric (Dir {conc=Sym _}) = True
isSymmetric _                  = False

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
mean (Dir {conc=Sym _})                  = Nothing
mean (Dir {conc=Asym a, precision=prec}) = Just $ EM.toList $ fmap (/ prec) a

-- | Symmetrize a Dirichlet distribution such that mean=0 yet preserving precision
symmetrize :: Enum a => Dirichlet a -> Dirichlet a
symmetrize alpha@(Dir {conc=Sym _}) = alpha
symmetrize alpha =
  let a = alpha { conc = Sym $ precision alpha / realToFrac (dimension alpha)
                , norm = computeNorm a
                }
  in a

-- | Pretty-print a Dirichlet prior
prettyPrint :: Enum a => (a -> String) -> Dirichlet a -> Doc
prettyPrint showA (Dir {conc=Sym alpha}) = text "Symmetric" <+> double alpha
prettyPrint showA (Dir {conc=Asym alphas}) =
  text "Assymmetric"
  <+> fsep (punctuate comma
           $ map (\(a,alpha)->text (showA a) <> parens (text $ printf "%1.2e" alpha))
           $ take 100 $ EM.toList $ alphas)
