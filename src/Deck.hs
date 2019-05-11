-- Like a deck of cards.
module Deck
    ( Deck
    , fromFoldable
    , Location
    , draw
    , insert
    , drawFrom
    , insertAt
    , top
    , bottom
    , randomLocation
    ) where

import Control.Monad.Random
import Data.List (uncons)
import Data.Foldable (toList)
import Random (unconsRandom, consRandom)

-- TODO: Update `Deck` to use a more suitable data structure.
newtype Deck a = Deck { unpack :: [a] }
  deriving (Semigroup, Monoid, Functor, Foldable, Applicative)

-- TODO: Try to find a way to wrap up `Location` as a type without
--       any variables, so that the deck can help users out by figuring
--       out the right values of m and a.
data Location m a = Location { picker :: Picker m a
                             , insertion :: Insertion m a }

top :: Location m a
top = Location fromTop toTop

bottom :: Location m a
bottom = Location fromBottom toBottom

randomLocation :: MonadRandom m => Location m a
randomLocation = Location fromRandom toRandom

draw :: Applicative m => Deck a -> m (Maybe (a, Deck a))
draw = drawFrom top

insert :: Applicative m => a -> Deck a -> m (Deck a)
insert = insertAt bottom

-- TODO: Consider merging `Maybe` into the computational context.
-- TODO: Figure out a way to avoid writing `Applicative` in as many
--       places.
drawFrom :: Applicative m => Location m a ->
                             Deck a -> m (Maybe (a, Deck a))
drawFrom location (Deck xs) = tidy $ (picker location) xs
  where tidy = (fmap . fmap . fmap) Deck

insertAt :: Applicative m => Location m a ->
                             a -> Deck a -> m (Deck a)
insertAt location x (Deck xs) = tidy $ (insertion location) x xs
  where tidy = fmap Deck

fromFoldable :: Foldable f => f a -> Deck a
fromFoldable = Deck . toList

-- Ways of picking cards from the deck.
type Picker m a = Applicative m => [a] -> m (Maybe (a, [a]))

fromTop :: Picker m a
fromTop = pure . uncons

-- TODO: Define insertion at bottom.
fromBottom :: Picker m a
fromBottom = undefined

fromRandom :: MonadRandom m => Picker m a
fromRandom = unconsRandom

-- TODO: ways of inserting cards into the deck.
type Insertion m a = Applicative m => a -> [a] -> m [a]

toTop :: Insertion m a
toTop x xs = pure (x:xs)

toBottom :: Insertion m a
toBottom x xs = pure (xs ++ [x])

toRandom :: MonadRandom m => Insertion m a
toRandom = consRandom