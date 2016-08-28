module News.Feed
( Feed
, EntryList(..)
, Entry
, cache
, limit
) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Control.Monad.Eff.Ref (newRef, readRef, REF, writeRef)
import Data.JSDate (JSDate)
import Data.List as List
import News.Prelude

type Feed eff =
  { title :: String
  , url   :: String
  , fetch :: Aff eff EntryList
  }

data EntryList
  = GenericEntryList (List Entry)
  | TwitterEntryList String

type Entry =
  { title :: String
  , url   :: String
  , time  :: JSDate
  }

cache
  :: forall eff m
   . (MonadEff (ref :: REF | eff) m)
  => Feed (ref :: REF | eff)
  -> m (Feed (ref :: REF | eff))
cache feed = do
  ref <- liftEff $ newRef Nothing
  pure feed { fetch = cached ref }
  where cached ref = do
          old <- liftEff $ readRef ref
          case old of
            Just entries -> pure entries
            Nothing -> do
              new <- feed.fetch
              liftEff $ writeRef ref (Just new)
              pure new

limit :: forall eff. Int -> Feed eff -> Feed eff
limit n feed = feed { fetch = go }
  where go = feed.fetch <#> case _ of
               GenericEntryList l -> GenericEntryList $ List.take n l
               TwitterEntryList t -> TwitterEntryList t
