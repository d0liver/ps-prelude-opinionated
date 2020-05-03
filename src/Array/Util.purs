module Array.Util (
  (:>), (<:)
) where

import Data.Array (cons, snoc)

infixl 3 snoc as :>
infixl 3 cons as <:
