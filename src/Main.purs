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
main' = withRouting (index <|> notFound)

index :: forall eff m. (MonadReader (Request eff) m, MonadZero m) => m (Response eff)
index = root *> render 200 "Home" \w -> pure unit

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
