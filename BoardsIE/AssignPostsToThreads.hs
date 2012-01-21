{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
import Control.Monad
import Database.Redis
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS

connectInfo = defaultConnectInfo {connectHost="blake"}
main =
  do conn <- connect $ connectInfo
     runRedis conn $ do Right threads <- smembers "%threads"
                        forM_ threads $ \t->do
                          Right posts <- smembers $ t `BS.append` "%posts"
                          forM_ posts $ \p->hset p "thread" t

