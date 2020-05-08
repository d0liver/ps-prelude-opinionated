module Prelude.Opinionated.Either.Util where

import Prelude
import Data.Either (Either(..), either)

prefixLeft :: ∀ a. String -> Either String a -> Either String a
prefixLeft pref (Left s) = Left (pref <> s)
prefixLeft pref r = r

whenLeft :: ∀ a m. Applicative m => Either String a -> (String -> m a) -> m a
whenLeft eth f = either f pure eth
