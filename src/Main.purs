module Main
( main
) where

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Cowlaser.Route (root, withRouting)
import Cowlaser.Serve (nodeHandler)
import News.Feed (cache, Feed, limit)
import News.Feed.RSS (rss)
import News.Page.Home (home)
import News.Page.NotFound (notFound)
import News.Prelude
import Node.HTTP (createServer, listen)

main :: forall eff. Eff (http :: HTTP, ref :: REF | eff) Unit
main = do
  releases'      <- cache $ limit 10 $ releases
  reddit'        <- cache $ limit 10 $ reddit
  google'        <- cache $ limit 10 $ google
  stackOverflow' <- cache $ limit 10 $ stackOverflow
  twitter'       <- cache $ limit 10 $ twitter
  let feeds = releases' : reddit' : google' : stackOverflow' : twitter' : Nil

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
        google =
          { title: "Google Groups"
          , url: "https://groups.google.com/group/purescript"
          , fetch: rss "https://groups.google.com/forum/feed/purescript/topics/atom.xml?num=15"
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
main' feeds = withRouting go
  where go =     (root *> home feeds)
             <|> (notFound)
