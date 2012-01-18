module Extract where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Control.Exception as E

import Control.Concurrent.ParallelIO.Global
import qualified System.FilePath.Find as FP
import System.FilePath.Find hiding (find)
import Database.Redis
import System.IO
import System.Environment

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

connectInfo = defaultConnectInfo {connectHost="blake"}
rootPath = "/iesl/local/dietz/boardsie"

extract :: (FilePath -> Redis ()) -> IO ()
extract handler =
  do dirs <- getArgs
     conn <- connect $ connectInfo
     --dirs <- liftM tail $ FP.find (depth <=? 0) (fileType ==? Directory &&? depth ==? 1) "."
     --let dirs = ["."]
     parallel_ $ map (handleDir conn handler) dirs
     stopGlobalPool

handleDir :: Connection -> (FilePath -> Redis ()) -> FilePath -> IO ()
handleDir conn handler d =
  do fs <- FP.find always (fileType ==? RegularFile) d
     runRedis conn $
       forM_ (zip [1..] fs) $ \(i,f)-> do
         handler f
         when (i `mod` 1000 == 0) $ liftIO $ do
           putStr $ d ++ "\t"
           putStrLn $ show i
           hFlush stdout

tryReadFile :: MonadIO m => FilePath -> MaybeT m Text
tryReadFile fp =
  MaybeT $ liftIO $ E.catch (TIO.readFile fp >>= return . Just)
                            (\e->do print (e :: IOError)
                                    return Nothing)

