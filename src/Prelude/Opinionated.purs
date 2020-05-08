module Prelude.Opinionated (
    module X
  , errorName, errorStack, errorMessage
  , takePair, P(..)
  , inList, (<~)
) where

import Partial.Unsafe (unsafeCrashWith)
import Prelude.Unicode ((∘))
import Data.Array as A
import Data.List (List)
import Data.List as L

-- Re-exports
import Prelude.Opinionated.Aff.Util (affDie, affLog, die, ε) as X
import Data.String.Regex.Unsafe (unsafeRegex) as X
import Data.String.Regex (Regex, regex, search, match, test, replace) as X
import Data.String.Regex.Flags (RegexFlags, noFlags, global) as X
import Data.Either (Either(..), choose, either, fromLeft, fromRight, hush, isLeft, isRight, note, note') as X
import Data.Foldable (findMap, foldM, indexl, indexr, length, null, oneOfMap, surround, surroundMap) as X
import Data.List (List) as X
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as X
import Prelude.Opinionated.Maybe.Util (whenNothing, whenJust, whenJustM, whenNothingM) as X
import Prelude.Opinionated.Either.Util (prefixLeft, whenLeft) as X
import Prelude.Opinionated.Array.Util ((:>), (<:)) as X
import Data.Traversable (class Foldable, class Traversable, Accum, all, and, any, elem, find, fold, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, for, for_, intercalate, mapAccumL, mapAccumR, maximum, maximumBy, minimum, minimumBy, notElem, oneOf, or, product, scanl, scanr, sequence, sequenceDefault, sequence_, sum, traverse, traverseDefault, traverse_) as X
import Effect (Effect, forE, foreachE, untilE, whileE) as X
import Effect.Aff (Aff) as X
import Effect.Class (class MonadEffect, liftEffect) as X
import Effect.Console (clear, errorShow, info, infoShow, log, logShow, time, timeEnd, timeLog, warn, warnShow) as X
import Effect.Exception (Error, catchException, error, throw, throwException, try) as X
import Data.Maybe (Maybe)
import Effect.Exception (Error)
import Effect.Exception as Error
import Partial.Unsafe (unsafeCrashWith, unsafePartial, unsafePartialBecause) as X
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Monoid, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, div, eq, flap, flip, gcd, identity, ifM, join, lcm, liftA1, liftM1, map, max, mempty, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as X
import Prelude.Unicode (type (⟿), (÷), (↢), (↣), (∘), (∣), (∤), (∧), (∨), (≠), (≡), (≢), (≤), (≥), (≮), (≯), (⊙), (⊛), (⊻), (⊼), (⊽), (⋅), (⋘), (⋙), (◇), (⤛), (⤜)) as X
import Foreign (MultipleErrors, fail, isArray, isNull, isUndefined, readArray, readBoolean, readChar, readInt, readNull, readNullOrUndefined, readNumber, readString, readUndefined, renderForeignError, tagOf, typeOf, unsafeFromForeign, unsafeReadTagged, unsafeToForeign) as X
import Foreign.Generic (class Decode, class Encode, class GenericDecode, class GenericEncode, F, Foreign, ForeignError(..), Options, SumEncoding(..), decode, decodeJSON, defaultOptions, encode, encodeJSON, genericDecode, genericDecodeJSON, genericEncode, genericEncodeJSON) as X
import Prelude.Opinionated.Foreign.Util (renderForeignErrors) as X
import Data.Generic.Rep (class Generic) as X
import Control.Promise (Promise) as X
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT) as X
import Control.Monad.Except (runExcept) as X
import Control.Monad.Except.Trans (runExceptT) as X
import Data.List.NonEmpty (NonEmptyList) as X
import Data.Array.NonEmpty (NonEmptyArray) as X

errorName :: Error -> String
errorName = Error.name
errorStack :: Error -> Maybe String
errorStack = Error.stack
errorMessage :: Error -> String
errorMessage = Error.message

inList :: ∀ a. (List a -> List a) -> Array a -> Array a
inList f = A.fromFoldable ∘ f ∘ L.fromFoldable

data P a = None | One a | Two a a

takePair :: ∀ a. Array a -> P a
takePair =
        takePairOrCrash ∘ (A.take 2)
    where
    takePairOrCrash [] = None
    takePairOrCrash [ a ] = One a
    takePairOrCrash [ a, b ] = Two a b
    takePairOrCrash _ = unsafeCrashWith "This should never happen!"

-- Reminder: It's convenient to ctrl-] to other operators directly to see their
-- precedence without leaving the editor
infixl 6 inList as <~
