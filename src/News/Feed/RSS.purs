module News.Feed.RSS
( rss
) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (throwError)
import Data.List as List
import News.Feed (Entry, EntryList(..))
import News.Prelude
import Node.HTTP (HTTP)
import Node.HTTP.Aff (request)

rss :: forall eff. String -> Aff (http :: HTTP | eff) EntryList
rss url = do
  text <- request url
  case parse Left Right url text of
    Right entries -> pure $ GenericEntryList (List.fromFoldable entries)
    Left error -> throwError error

foreign import parse
  :: (forall a b. a -> Either a b)
  -> (forall a b. b -> Either a b)
  -> String
  -> String
  -> Either Error (Array Entry)
