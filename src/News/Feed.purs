module News.Feed
( Feed
, EntryList(..)
, Entry
, cache
, limit
) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
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
  lastFetch <- liftEff $ newRef bottom
  list      <- liftEff $ newRef Nothing
  pure feed { fetch = cached lastFetch list }
  where cached lastFetch list = do
          oldLastFetch <- liftEff $ readRef lastFetch
          newLastFetch <- liftEff $ (now :: Eff (ref :: REF | eff) Int)
          when (newLastFetch - oldLastFetch > 180000) $ do
            liftEff $ writeRef lastFetch newLastFetch
            liftEff $ writeRef list Nothing

          oldList <- liftEff $ readRef list
          case oldList of
            Just entries -> pure entries
            Nothing -> do
              newList <- feed.fetch
              liftEff $ writeRef list (Just newList)
              pure newList

limit :: forall eff. Int -> Feed eff -> Feed eff
limit n feed = feed { fetch = go }
  where go = feed.fetch <#> case _ of
               GenericEntryList l -> GenericEntryList $ List.take n l
               TwitterEntryList t -> TwitterEntryList t

foreign import now :: forall eff. Eff eff Int
