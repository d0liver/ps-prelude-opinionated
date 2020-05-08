module Prelude.Opinionated.Aff.Util where

-- We can't depend on Primacy.Prelude here because this module is included in
-- the prelude

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log, warn)
import Effect.Exception (throw)
import Node.Process as Proc
import Prelude.Unicode ((∘))

affDie :: ∀ a. String -> Aff a
affDie = ε ∘ die

affThrow :: forall m a. MonadEffect m => String -> m a
affThrow = ε ∘ throw

affLog :: forall a. MonadEffect a => String -> a Unit
affLog = ε ∘ log

die :: ∀ a. String -> Effect a
die msg = warn msg *> Proc.exit 1

ε :: forall a m. MonadEffect m => Effect a -> m a
ε = liftEffect
