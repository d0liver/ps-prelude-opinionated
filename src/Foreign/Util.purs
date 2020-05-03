module Foreign.Util where

import Prelude
import Data.Either (Either(..))

import Control.Monad.Except (runExcept)
import Data.Foldable (intercalate)
import Foreign (F, renderForeignError)

renderForeignErrors :: ∀ a. F a -> Either String a
renderForeignErrors f =
  case runExcept f of
    Left errs -> Left (intercalate "\n" (renderForeignError <$> errs))
    Right r -> Right r
