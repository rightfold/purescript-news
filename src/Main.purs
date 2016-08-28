module Main
( main
) where

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Error.Class (catchError)
import Control.MonadZero (class MonadZero)
import Cowlaser.HTTP (statusNotFound, statusOK)
import Cowlaser.Route (root, withRouting)
import Cowlaser.Serve (nodeHandler)
import Data.Map as Map
import News.Feed (cache, Feed)
import News.Feed.RSS (rss)
import News.Prelude
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (createServer, HTTP, listen)
import Node.Stream (Writable)
import Node.Stream.Aff as Stream

main :: forall eff. Eff (http :: HTTP, ref :: REF | eff) Unit
main = do
  releases' <- cache releases
  reddit' <- cache reddit
  twitter' <- cache twitter
  stackOverflow' <- cache stackOverflow
  let feeds = releases' : reddit' : twitter' : stackOverflow' : Nil

  server <- createServer $ nodeHandler (main' feeds)
  listen server 8000 (pure unit)

  where releases =
          { title: "Releases"
          , url: "https://github.com/purescript/purescript/releases"
          , fetch: rss "https://github.com/purescript/purescript/releases.atom"
          }

        reddit =
          { title: "Reddit"
          , url: "https://www.reddit.com/r/purescript"
          , fetch: rss "https://www.reddit.com/r/purescript.rss"
          }
        twitter =
          { title: "Twitter"
          , url: "https://twitter.com/purescript"
          , fetch: pure Nil
          }
        stackOverflow =
          { title: "Stack Overflow"
          , url: "https://stackoverflow.com/questions/tagged/purescript"
          , fetch: rss "https://stackoverflow.com/feeds/tag?tagnames=purescript&sort=newest"
          }

main'
  :: forall eff m
   . (MonadReader (Request eff) m)
  => List (Feed (http :: HTTP | eff))
  -> m (Response eff)
main' feeds = withRouting (index feeds <|> notFound)

index
  :: forall eff m
   . (MonadReader (Request eff) m, MonadZero m)
  => List (Feed (http :: HTTP | eff))
  -> m (Response eff)
index feeds = root *> render statusOK "Home" \w -> do
  write w "<section class=\"-feeds\">"
  for_ feeds \feed -> do
    write w "<article class=\"-feed\"><h1><a href=\""
    write w (html feed.url)
    write w "\" rel=\"nofollow\">"
    write w (html feed.title)
    write w "</a></h1>"

    entries' <- catchError (Right <$> feed.fetch) (pure <<< Left)
    case entries' of
      Left (err :: Error) -> do
        write w "<p>Failed to fetch feed</p><pre>"
        write w (html (show err))
        write w "</pre>"
      Right entries -> do
        write w "<ol>"
        for_ entries \entry -> do
          write w "<li><a href=\""
          write w (html entry.url)
          write w "\" rel=\"nofollow\">"
          write w (html entry.title)
          write w "</a> &mdash; <time>"
          write w (html entry.time)
          write w "</time></li>"
        write w "</ol>"

    write w "</article>"
  write w "</section>"

notFound :: forall eff m. (MonadReader (Request eff) m) => m (Response eff)
notFound = do
  uri <- _.uri <$> ask
  render statusNotFound "Not Found" \w -> do
    write w """
      <h1>Not Found</h1>
      <p>The requested page could not be found.</p>
      <pre>"""
    write w (html uri)
    write w "</pre>"

render
  :: forall eff m
   . (Applicative m)
  => {code :: Int, message :: String}
  -> String
  -> (Writable () (http :: HTTP | eff) -> Aff (http :: HTTP | eff) Unit)
  -> m (Response eff)
render status title body =
  pure { status
       , headers: Map.singleton (CI "content-type") ("text/html" :| Nil)
       , body: \w -> header w *> body w *> footer w *> Stream.end w
       }
  where header w = do
          write w """
            <!DOCTYPE html>
            <html>
              <head>
                <meta charset="utf-8">
                <title>"""
          write w (html title)
          write w """
                </title>
                <style>
                  @import 'https://fonts.googleapis.com/css?family=Roboto';

                  body {
                    margin: 0;
                    font-family: Roboto;
                    font-size: 16px;
                    color: #4d4d4d; }

                  body > .-header {
                    background: #14161A;
                    color: white;
                    font-family: Roboto, sans-serif;
                    font-weight: bold;
                    font-size: 15px;
                    letter-spacing: 3px;
                    text-transform: uppercase;
                    padding: 32px; }

                  body > .-feeds {
                    padding: 32px;
                    display: flex;
                    flex-wrap: wrap; }
                    body > .-feeds > .-feed {
                      flex: 1 0 500px; }
                      body > .-feeds > .-feed > h1 {
                        color: #111;
                        font-family: Roboto, sans-serif;
                        font-size: 15px;
                        letter-spacing: 3px;
                        text-transform: uppercase;
                        margin: 0 0 22px 0;
                        line-height: 22px; }
                        body > .-feeds > .-feed > h1 > a {
                          color: #111;
                          text-decoration: none; }
                      body > .-feeds > .-feed > ol {
                        margin: 22px 0;
                        padding: 0;
                        list-style: none; }
                        body > .-feeds > .-feed > ol > li {
                          line-height: 22px; }
                          body > .-feeds > .-feed > ol > li > a {
                            color: #c4953a;
                            text-decoration: none; }
                          body > .-feeds > .-feed > ol > li > a:hover,
                          body > .-feeds > .-feed > ol > li > a:visited {
                            color: #e8d5b0; }
                </style>
              </head>
              <body>
                <header class="-header">
                  PureScript News
                </header>"""
        footer w = write w "</body></html>"

foreign import html :: String -> String

write w s = Stream.writeString w UTF8 s
