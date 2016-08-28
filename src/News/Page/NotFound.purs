module News.Page.NotFound
( notFound
) where

import Cowlaser.HTTP (statusNotFound)
import News.Page (escapeHTML, render)
import News.Prelude
import Node.Encoding (Encoding(UTF8))
import Node.Stream.Aff as Stream

notFound :: forall eff m. (MonadReader (Request eff) m) => m (Response eff)
notFound = do
  uri <- _.uri <$> ask
  render statusNotFound "Not Found" \w -> do
    write w """
      <h1>Not Found</h1>
      <p>The requested page could not be found.</p>
      <pre>"""
    write w (escapeHTML uri)
    write w "</pre>"

write w s = Stream.writeString w UTF8 s
