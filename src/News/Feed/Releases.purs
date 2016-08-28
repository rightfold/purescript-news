module News.Feed.Releases
( releases
) where

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.List as List
import News.Feed (Entry, Feed)
import News.Prelude
import Node.HTTP (HTTP)
import Node.HTTP.Aff (request)

releases :: forall eff. Feed (http :: HTTP | eff)
releases =
  { title: "Releases"
  , url: "https://github.com/purescript/purescript/releases"
  , fetch: request "https://api.github.com/repos/purescript/purescript/releases"
           >>= parse
           <#> List.take 10
  }

parse :: forall m. (MonadError Error m) => String -> m (List Entry)
parse json =
  pure json >>= F.readJSON >>= F.readArray >>= traverse (\jEntry -> do
    title <- F.readProp "name"     jEntry
    url   <- F.readProp "html_url" jEntry
    pure {title, url})
  # case _ of
      Left err -> throwError (error (show err))
      Right es -> pure (List.fromFoldable es)
