module Node.HTTP.Aff
( request
) where

import Control.Monad.Aff (Aff)
import Node.HTTP (HTTP)

foreign import request :: forall eff. String -> Aff (http :: HTTP | eff) String
