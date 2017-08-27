module Common.ObjectId
 where

import Data.Text
import qualified Data.Aeson as A
import Data.Bson (ObjectId)
import Control.Monad (mzero)

instance A.ToJSON ObjectId where
  toJSON = A.toJSON . pack . show

instance A.FromJSON ObjectId where
  parseJSON (A.String v) = return . read . unpack $ v
  parseJSON _ = mzero
