import           Control.Monad                
import           Control.Applicative                

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString as BS

import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import           Data.Serialize

import           BayesStack.DirMulti
import           BayesStack.Models.Topic.LDA
import           ReadData       
                 
readItemMap :: IO (M.Map Item Term)                 
readItemMap =
    (either error id . runGet get) <$> BS.readFile "sweeps/node-map"

readSweep :: FilePath -> IO MState
readSweep fname = (either error id . runGet get) <$> BS.readFile fname

readNetData :: FilePath -> IO NetData
readNetData fname = (either error id . runGet get) <$> BS.readFile fname

main = do
    d <- readNetData "sweeps/data"
    itemMap <- readItemMap
    m <- readSweep "sweeps/00010"
    print itemMap
    forM (S.toList $ dTopics d) $ \t->do
        print $ prettyMultinom 30 (show . (itemMap M.!)) $ stPhis m M.! t

instance Serialize T.Text where
     put = put . TE.encodeUtf8
     get = TE.decodeUtf8 <$> get
