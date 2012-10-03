module SerializeText () where

import           Control.Applicative       
import           Data.Serialize
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- FIXME: Why isn't there already an instance?
instance Serialize T.Text where
     put = put . TE.encodeUtf8
     get = TE.decodeUtf8 <$> get