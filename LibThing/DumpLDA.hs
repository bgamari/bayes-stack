import Control.Monad (liftM)
import Control.Monad.Trans.State
import Data.Function (on)

import Data.Traversable
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.ByteString as BS
import qualified Data.EnumMap as EM

import Data.Serialize
import BayesStack.Core
import BayesStack.DirMulti
import System.Environment
import System.IO

import LibThing.Data
import BayesStack.Models.Topic.Types
import BayesStack.Models.Topic.LDA

import Text.CSV
import Text.PrettyPrint
import Text.Printf

maybeInc Nothing = Just 1
maybeInc (Just n) = Just (n+1)

addCat :: (a -> Probability) -> (a -> Probability) -> Double -> Double -> (a -> Probability)
addCat f g a b x = a*(f x) + b*(g x)

getUserGroups :: LDAModelState -> IO (EM.EnumMap Node (Set Group), [Group])
getUserGroups state =
  do records <- readUserGroups
     let a = EM.fromListWith S.union $ map (\(n,g)->(n, S.singleton g)) records
     return (a, nub $ map snd records)

pearsonCorr :: [(Double, Double)] -> Double
pearsonCorr s =
  let n = realToFrac $ length s
      mean xs = sum xs / n
      std xs = sqrt (sum $ map (\x->(x-mean xs)^2 / n) xs)
      (xs,ys) = unzip s
  in 1/(n-1) * sum (map (\(x,y)->(x-mean xs)/std xs * (y-mean ys)/std ys) s)

topicGroupCorr :: LDAModelState -> EM.EnumMap Node (Set Group) -> Topic -> Group -> Double
topicGroupCorr state userGroups topic group =
    pearsonCorr $ map (\u->(prob (msThetas state EM.! u) topic, weight u))
  $ S.toList $ ldaNodes $ msData state
  where weight u | group `S.member` (userGroups EM.! u) =
                                1 / realToFrac (S.size $ userGroups EM.! u)
                 | otherwise = 0

main =
  do --a <- liftM decode $ BS.readFile "word.map"
     --let wordMap :: EM.EnumMap Item String
     --    wordMap = either (error "Bad word map") id a
     f:_ <- getArgs
     Right state <- liftM decode $ BS.readFile f
     print $ msLogLikelihood state

     (userGroups, groups) <- getUserGroups state

     let printCorr (t,g,c) = printf "%s\t%s\t%1.2e" (show t) (show g) c
     writeFile "node.corr" $ unlines $ map printCorr $
        do t <- S.toList $ ldaTopics $ msData state
           g <- groups
           let c = topicGroupCorr state userGroups t g
           return (t,g,c)

     putStrLn "P(x|t)"
     forM (S.toList $ ldaTopics $ msData state) $ \t ->
       do let phi = msPhis state EM.! t
          print $ prettyDirMulti 10 (show) phi

     putStrLn "\nP(t|x)"
     let wordCounts :: EM.EnumMap Item Int
         wordCounts = execState (forM (ldaNodeItems $ msData state) $ \(n,x)->
                                   modify $ EM.alter maybeInc x
                                ) EM.empty
     let totalCounts = EM.size $ ldaNodeItems $ msData state
     forM (S.toList $ ldaTopics $ msData state) $ \t ->
       do let phi = msPhis state EM.! t
          let probs = map (\x->prob phi x / realToFrac (wordCounts EM.! x * totalCounts))
                      $ S.toList $ ldaItems $ msData state
          print $ text (show t) <+> colon
              <+> hsep (punctuate comma
                        $ map (\(x,p)->text (show x) <> parens (text $ printf "%1.2e" p))
                        $ take 10 $ sortBy (flip (compare `on` snd))
                        $ zip (S.toList $ ldaItems $ msData state) probs
                       )

