module News.Page
( render
, escapeHTML
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
          write w (escapeHTML ("PureScript News - " <> title))
          write w """
                </title>
                <style>
                  @import 'https://fonts.googleapis.com/css?family=Roboto';

                  body {
                    margin: 0;
                    font-family: Roboto;
                    font-size: 16px;
                    color: #4d4d4d; }

                  body > .-header, body > .-footer {
                    background: #14161A;
                    color: white;
                    font-size: 15px;
                    padding: 22px; }

                  body > .-header {
                    font-weight: bold;
                    letter-spacing: 3px;
                    text-transform: uppercase; }

                  body > .-footer > a {
                    color: white;
                    text-decoration: none;
                    margin-right: 22px; }
                    body > .-footer > a:hover {
                      text-decoration: underline; }

                  body > .-feeds {
                    padding: 32px;
                    display: flex;
                    flex-wrap: wrap; }
                    @media screen and (max-width: 1000px) {
                      body > .-feeds > .-feed {
                        flex: 1 0 100%; }
                    }
                    @media screen and (min-width: 1000px) {
                      body > .-feeds > .-feed {
                        flex: 1 0 50%; }
                    }
                    @media screen and (min-width: 1500px) {
                      body > .-feeds > .-feed {
                        flex: 1 0 33%; }
                    }
                      body > .-feeds > .-feed > h1 {
                        color: #111;
                        font-size: 15px;
                        letter-spacing: 3px;
                        text-transform: uppercase;
                        margin: 0 10px 22px 0;
                        padding: 0 10px;
                        line-height: 22px;
                        border-bottom: 1px solid #111; }
                        body > .-feeds > .-feed > h1 > a {
                          color: #111;
                          text-decoration: none; }
                      body > .-feeds > .-feed > ol {
                        margin: 22px 0;
                        padding: 0 10px;
                        list-style: none; }
                        body > .-feeds > .-feed > ol > li {
                          line-height: 22px;
                          clear: right; }
                          body > .-feeds > .-feed > ol > li > a {
                            color: #c4953a;
                            text-decoration: none; }
                          body > .-feeds > .-feed > ol > li > a:hover,
                          body > .-feeds > .-feed > ol > li > a:visited {
                            color: #e8d5b0; }
                          body > .-feeds > .-feed > ol > li > time {
                            float: right; }
                      body > .-feeds > .-feed > .-more {
                        margin: 22px 0;
                        line-height: 22px; }
                        body > .-feeds > .-feed > .-more > a {
                          color: #c4953a;
                          text-transform: uppercase;
                          font-weight: bold;
                          letter-spacing: 1px;
                          font-size: 10px;
                          text-decoration: none; }
                </style>
              </head>
              <body>
                <header class="-header">
                  PureScript News
                </header>"""
        footer w = write w """
                <footer class="-footer">
                  <a href="http://www.purescript.org/">PureScript</a>
                  <a href="https://github.com/rightfold/purescript-news">GitHub</a>
                </footer>
              </body>
            </html>"""

foreign import escapeHTML :: String -> String

write w s = Stream.writeString w UTF8 s
