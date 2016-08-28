module Main
( main
) where

import Control.Monad.Eff (Eff)
import Cowlaser.Serve (nodeHandler)
import Data.Map as Map
import News.Prelude
import Node.HTTP (createServer, HTTP, listen)
import Node.Stream.Aff as Stream

main :: forall eff. Eff (http :: HTTP | eff) Unit
main = do
  server <- createServer $ nodeHandler main'
  listen server 8000 (pure unit)

main' :: forall eff m. (MonadReader (Request eff) m) => m (Response eff)
main' = pure { status: {code: 200, message: "OK"}
             , headers: Map.singleton (CI "content-type") ("text/html" :| Nil)
             , body: \w -> Stream.end w
             }
