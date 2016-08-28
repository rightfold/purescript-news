module News.Page.Home
( home
) where

import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (catchError)
import Control.Parallel.Class (parTraverse)
import Cowlaser.HTTP (statusOK)
import Data.JSDate (JSDate)
import News.Feed (Feed)
import News.Page (html, render)
import News.Prelude
import Node.Encoding (Encoding(UTF8))
import Node.Stream.Aff as Stream

home
  :: forall eff m
   . (MonadReader (Request eff) m)
  => List (Feed (http :: HTTP | eff))
  -> m (Response eff)
home feeds = render statusOK "Home" \w -> do
  feeds' <- feeds # parTraverse \feed ->
    {feed, entries: _} <$> catchError (Right <$> feed.fetch) (pure <<< Left)

  write w "<section class=\"-feeds\">"
  for_ feeds' \{feed, entries: entries'} -> do
    write w "<article class=\"-feed\"><h1><a href=\""
    write w (html feed.url)
    write w "\" rel=\"nofollow\">"
    write w (html feed.title)
    write w "</a></h1>"

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
          write w (html (timeAgo entry.time))
          write w "</time></li>"
        write w "</ol>"

    write w "<p class=\"-more\"><a href=\""
    write w (html feed.url)
    write w "\" rel=\"nofollow\">View More</a></p>"

    write w "</article>"
  write w "</section>"

write w s = Stream.writeString w UTF8 s

-- FIXME: this function is impure
foreign import timeAgo :: JSDate -> String
