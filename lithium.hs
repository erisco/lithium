--
-- A proof-of-concept Haskell library built on Sodium to demonstrate
-- that Behaviour can generalise Event.
--
-- Author: Eric Brisco (eric.brisco@gmail.com)
--
-- Sodium is authored by Stephen Blackheath.
--
{-# LANGUAGE FlexibleInstances #-}
module FRP.Lithium where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Arrow ((&&&))
import Data.Monoid
import qualified FRP.Sodium.Context as Na
import FRP.Sodium.Context (Context, Reactive)
import qualified FRP.Sodium as NaP
import FRP.Sodium (Plain)

-- Generalised type over behaviours and events.
newtype Behaviour r a = Behaviour (Reactive r (Na.Behaviour r a))

instance (Context r) => Functor (Behaviour r) where
  fmap f (Behaviour r) = Behaviour $ fmap (fmap f) r
--

instance (Context r) => Applicative (Behaviour r) where
  pure = wrap . fmap fst . Na.newBehaviour
  x <*> y = wrap $ (liftA2 (<*>)) (unwrap x) (unwrap y)
--


-- An attempt to make a Monad instance but it falls short.
--instance (Context r) => Monad (Behaviour r) where
--  return = pure
--  -- gets to Reactive r (Na.Event r a)
--  x >>= f = wrap . join . fmap (join . fmap (Na.switchE . fmap (maybe Na.never id))
--    . unwrap . fromEvent . Na.execute . Na.value . fmap (fmap Na.value . unwrap . f)) . unwrap $ x
----


type Event r a = Behaviour r (Maybe a)

-- Helpers

unwrap :: Behaviour r a -> Reactive r (Na.Behaviour r a)
unwrap (Behaviour r) = r

wrap :: Reactive r (Na.Behaviour r a) -> Behaviour r a
wrap = Behaviour

-- General map from a Behaviour to an Event.
behaviourToEvent :: (Context r) => (Na.Event r a -> Na.Event r b) -> Behaviour r a -> Event r b
behaviourToEvent f = wrap . join . fmap (unwrap . fromEvent . f . Na.value) . unwrap


-- Conversions to/from Sodium. The key is that, in the Reactive monad,
-- 'Event a' is isomorphic to 'Behaviour (Maybe a)'.

fromBehaviour :: (Context r) => Na.Behaviour r a -> Behaviour r a
fromBehaviour = wrap . return

toBehaviour :: (Context r) => Behaviour r a -> Reactive r (Na.Behaviour r a)
toBehaviour = unwrap

fromEvent :: (Context r) => Na.Event r a -> Event r a
fromEvent = wrap . Na.hold Nothing . fmap Just

toEvent :: (Context r) => Event r a -> Reactive r (Na.Event r a)
toEvent = fmap (Na.filterJust . Na.value) . unwrap


-- Equivalent core functions

instance (Context r) => Monoid (Event r a) where
  mempty = fromEvent Na.never
  mappend rx ry = wrap $ do
    x <- toEvent rx
    y <- toEvent ry
    unwrap . fromEvent $ Na.merge x y
--

never :: (Context r) => Event r a
never = mempty

merge :: (Context r) => Event r a -> Event r a -> Event r a
merge = mappend

filterJust :: (Context r) => Event r (Maybe a) -> Event r a
filterJust = wrap . fmap (fmap join) . unwrap

hold :: (Context r) => a -> Event r a -> Behaviour r a
hold x = wrap . fmap (fmap (maybe x id)) . unwrap

updates :: (Context r) => Behaviour r a -> Event r a
updates = wrap . (unwrap . fromEvent . Na.updates =<<) . unwrap

value :: (Context r) => Behaviour r a -> Event r a
value = wrap . (unwrap . fromEvent . Na.value =<<) . unwrap

snapshot :: (Context r) => (a -> b -> c) -> Event r a -> Behaviour r b -> Event r c
snapshot f e b = liftA2 f <$> e <*> fmap Just b

switchE :: (Context r) => Behaviour r (Event r a) -> Event r a
switchE b = let
  f x = (unwrap . fromEvent . Na.switchE =<<)
      . join
      . fmap (Na.hold x . Na.execute . Na.value . fmap toEvent)
      . unwrap
  in wrap $ sample b >>= toEvent >>= flip f b
--

switch :: (Context r) => Behaviour r (Behaviour r a) -> Behaviour r a
switch b = let
  f x = (Na.switch =<<)
      . join
      . fmap (Na.hold x . Na.execute . Na.value . fmap toBehaviour)
      . unwrap
  in wrap $ sample b >>= toBehaviour >>= flip f b
--

execute :: (Context r) => Event r (Reactive r a) -> Event r a
execute = wrap . join . fmap (unwrap . fromEvent . Na.execute) . toEvent

sample :: (Context r) => Behaviour r a -> Reactive r a
sample = join . fmap Na.sample . unwrap

coalesce :: (Context r) => (a -> a -> a) -> Event r a -> Event r a
coalesce f = fmap join . behaviourToEvent (Na.coalesce (liftA2 f))

once :: (Context r) => Event r a -> Event r a
once = fmap join . behaviourToEvent Na.once

split :: (Context r) => Event r [a] -> Event r a
split = behaviourToEvent (Na.split . fmap (maybe [] id))


-- Equivalent derived functions

newBehaviour :: (Context r) => a -> Reactive r (Behaviour r a, a -> Reactive r ())
newBehaviour = (return . (fromBehaviour . fst &&& snd) =<<) . Na.newBehaviour

-- issue: 'set (pure a)' is never propagated, only 'set empty' is. Not
--        sure if this is an issue with Sodium or not.
newEvent :: (Context r) => Reactive r (Event r a, a -> Reactive r ())
newEvent = let
  pulse :: (Context r, Alternative f) => (f a -> Reactive r ()) -> a -> Reactive r ()
  pulse set a = set (pure a) >> set empty
  in return . (fmap join . fromEvent . fst &&& pulse . snd) =<< Na.newEvent
--

-- rest omitted...


-- Added this because it is essential for examples
listen :: Event Plain a -> (a -> IO ()) -> Reactive Plain (IO ())
listen e act = toEvent e >>= flip NaP.listen act





