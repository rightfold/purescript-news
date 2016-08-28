module News.Page
( render
, html
) where

import Control.Monad.Aff (Aff)
import Data.Map as Map
import News.Prelude
import Node.Encoding (Encoding(UTF8))
import Node.Stream (Writable)
import Node.Stream.Aff as Stream

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
