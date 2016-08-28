module News.Feed
( Feed
, Entry
) where

import Control.Monad.Aff (Aff)
import News.Prelude

type Feed eff =
  { title :: String
  , url   :: String
  , fetch :: Aff eff (List Entry)
  }

type Entry =
  { title :: String
  , url   :: String
  }
