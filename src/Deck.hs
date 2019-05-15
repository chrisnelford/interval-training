-- Like a deck of cards.
module Deck
    ( Deck
    , fromFoldable
    , Location(..)
    , draw
    , insert
    , drawFrom
    , insertAt
    ) where

import Control.Monad.Random (MonadRandom)
import Data.List (uncons)
import Data.Foldable (toList)
import Random (unconsRandom, consRandom)

-- TODO: Update `Deck` to use a more suitable data structure.
newtype Deck a = Deck { unpack :: [a] }
  deriving (Semigroup, Monoid, Functor, Foldable, Applicative)

data Location m where
  Top :: Location m
  Bottom :: Location m
  Random :: MonadRandom m => Location m

draw :: Applicative m => Deck a -> m (Maybe (a, Deck a))
draw = drawFrom Top

insert :: Applicative m => a -> Deck a -> m (Deck a)
insert = insertAt Bottom

-- TODO: Consider merging `Maybe` into the computational context.
-- TODO: Figure out a way to avoid writing `Applicative` in as many
--       places.
drawFrom :: Applicative m => Location m ->
                             Deck a -> m (Maybe (a, Deck a))
drawFrom location (Deck xs) = tidy $ (picker location) xs
  where tidy = (fmap . fmap . fmap) Deck

insertAt :: Applicative m => Location m ->
                             a -> Deck a -> m (Deck a)
insertAt location x (Deck xs) = tidy $ (insertion location) x xs
  where tidy = fmap Deck

fromFoldable :: Foldable f => f a -> Deck a
fromFoldable = Deck . toList

-- Ways of picking cards from the deck.
--
-- This use of applicative is a bit obnoxious. As long as
-- you give me a type `m` of kind (* -> *) and a function
-- `forall a. a -> m a`, I should be able to fulfill the contract.
--
-- In a context where `m` is an Applicative or Monad, it makes
-- sense to use `pure`, but there's no reason to constrain the
-- embedding function unless the particular `Picker` needs a
-- context with more features.
--
-- TODO: Look into whether there's some weaker constraint that
--       captures the "embedding" part of `Applicative`, without
--       requiring the `Functor` and application parts.
--
--       Alternatively, define my own "Natural embedding"
--       typeclass, that uses `pure` for any applicative.
--
--       Alternatively, find a better way to abstract over
--       contexts.
--
--       Alternatively, decide this is not worth worrying
--       about.
--
-- TODO: Think more generally about how to express the existence
--       of embeddings in Haskell.
type Picker m a = Applicative m => [a] -> m (Maybe (a, [a]))

-- TODO: Define insertion at bottom.
picker :: Location m -> Picker m a
picker Top = pure . uncons
picker Bottom = undefined
picker Random = unconsRandom

-- Ways of inserting cards into the deck.
type Insertion m a = Applicative m => a -> [a] -> m [a]

insertion :: Location m -> Insertion m a
insertion Top = \x xs -> pure (x:xs)
insertion Bottom = \x xs -> pure (xs ++ [x])
insertion Random = consRandom