module News.Page.JSON
( json
) where

import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (catchError)
import Control.Parallel.Class (parTraverse)
import Cowlaser.HTTP (statusOK)
import Data.Argonaut.Core as J
import Data.JSDate (toUTCString)
import Data.List as List
import Data.Map as Map
import Data.StrMap as StrMap
import News.Feed (EntryList(..), Feed)
import News.Prelude
import Node.Encoding (Encoding(UTF8))
import Node.Stream.Aff as Stream

json
  :: forall eff m
   . (Applicative m)
  => List (Feed (http :: HTTP | eff))
  -> m (Response eff)
json feeds =
  pure { status: statusOK
       , headers: Map.singleton (CI "content-type") ("application/json" :| Nil)
       , body
       }
  where body w = do
          feeds' <- feeds # parTraverse \feed ->
            {feed, entries: _} <$> catchError (Right <$> feed.fetch) (pure <<< Left)
          Stream.writeString w UTF8 $ J.stringify (toJSON (List.toUnfoldable feeds'))
          Stream.end w

toJSON :: forall eff. Array {feed :: Feed eff, entries :: Either Error EntryList} -> J.Json
toJSON feeds = J.fromArray $ feeds # map \{feed, entries} ->
  J.fromObject $
    StrMap.empty
    # StrMap.insert "title"   (J.fromString feed.title)
    # StrMap.insert "url"     (J.fromString feed.url)
    # StrMap.insert "entries" case entries of
        Left err -> J.fromObject $
          StrMap.empty
          # StrMap.insert "status" (J.fromString "error")
          # StrMap.insert "error"  (J.fromString (show err))
        Right (GenericEntryList es) -> J.fromObject $
          StrMap.empty
          # StrMap.insert "status"  (J.fromString "ok")
          # StrMap.insert "type"    (J.fromString "generic")
          # StrMap.insert "entries" (J.fromArray $
            es # List.toUnfoldable # map \entry ->
              J.fromObject $
                StrMap.empty
                # StrMap.insert "title" (J.fromString entry.title)
                # StrMap.insert "url"   (J.fromString entry.url)
                # StrMap.insert "time"  (J.fromString (toUTCString entry.time)))
        Right (TwitterEntryList widget) -> J.fromObject $
          StrMap.empty
          # StrMap.insert "status" (J.fromString "ok")
          # StrMap.insert "type"   (J.fromString "twitter")
          # StrMap.insert "widget" (J.fromString widget)
