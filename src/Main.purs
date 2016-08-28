module Main
( main
) where

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.MonadZero (class MonadZero)
import Cowlaser.Route (root, withRouting)
import Cowlaser.Serve (nodeHandler)
import Data.Map as Map
import News.Feed (Feed)
import News.Prelude
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (createServer, HTTP, listen)
import Node.Stream (Writable)
import Node.Stream.Aff as Stream

main :: forall eff. Eff (http :: HTTP | eff) Unit
main = do
  server <- createServer $ nodeHandler main'
  listen server 8000 (pure unit)

main' :: forall eff m. (MonadReader (Request eff) m) => m (Response eff)
main' = withRouting (index feeds <|> notFound)
  where feeds = releases : reddit : twitter : stackOverflow : Nil
        releases =
          { title: "Releases"
          , url: "https://github.com/purescript/purescript/releases"
          , fetch: pure Nil
          }
        reddit =
          { title: "Reddit"
          , url: "https://www.reddit.com/r/purescript"
          , fetch: pure Nil
          }
        twitter =
          { title: "Twitter"
          , url: "https://twitter.com/purescript"
          , fetch: pure Nil
          }
        stackOverflow =
          { title: "Stack Overflow"
          , url: "https://stackoverflow.com/questions/tagged/purescript"
          , fetch: pure Nil
          }

index
  :: forall eff m
   . (MonadReader (Request eff) m, MonadZero m)
  => List (Feed (http :: HTTP | eff))
  -> m (Response eff)
index feeds = root *> render 200 "Home" \w ->
  for_ feeds \feed -> do
    Stream.writeString w UTF8 "<section><h1><a href=\""
    Stream.writeString w UTF8 (html feed.url)
    Stream.writeString w UTF8 "\" rel=\"nofollow\">"
    Stream.writeString w UTF8 (html feed.title)
    Stream.writeString w UTF8 "</a></h1></section>"

notFound :: forall eff m. (MonadReader (Request eff) m) => m (Response eff)
notFound = do
  uri <- _.uri <$> ask
  render 404 "Not Found" \w -> do
    Stream.writeString w UTF8 """
      <h1>Not Found</h1>
      <p>The requested page could not be found.</p>
      <pre>"""
    Stream.writeString w UTF8 (html uri)
    Stream.writeString w UTF8 "</pre>"

render
  :: forall eff m
   . (Applicative m)
  => Int
  -> String
  -> (Writable () (http :: HTTP | eff) -> Aff (http :: HTTP | eff) Unit)
  -> m (Response eff)
render status title body =
  pure { status: {code: status, message: ""}
       , headers: Map.singleton (CI "content-type") ("text/html" :| Nil)
       , body: \w -> header w *> body w *> footer w *> Stream.end w
       }
  where header w = do
          Stream.writeString w UTF8 """
            <!DOCTYPE html>
            <html>
              <head>
                <meta charset="utf-8">
                <title>"""
          Stream.writeString w UTF8 (html title)
          Stream.writeString w UTF8 "</title></head><body>"
        footer w = Stream.writeString w UTF8 "</body></html>"

foreign import html :: String -> String
