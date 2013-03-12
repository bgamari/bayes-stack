module ReadRelevanceData where

import           Control.Applicative
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           BayesStack.Models.Topic.LDARelevance
import           ReadData hiding (readNodeItems)
import           Data.Attoparsec.Text hiding (option)

readNodeItems :: Set Term -> FilePath
              -> IO (Either String (M.Map NodeName [(Term, ItemWeight)]))
readNodeItems stopWords fname =
    fmap (M.unionsWith (++)) . parseOnly (many1 line) <$> TIO.readFile fname
    where line = do doc <- takeTill isHorizontalSpace
                    words <- flip manyTill endOfLine $ do
                        skipSpace
                        word <- takeTill isHorizontalSpace
                        skipSpace
                        weight <- double
                        return (word, realToFrac weight)
                    return $ M.singleton doc words
