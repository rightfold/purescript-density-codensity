-- | Density comonad.
module Control.Comonad.Density
  ( DensityF (..)
  , Density (..)
  , mkDensity
  ) where

import Prelude hiding (apply, map, pure)

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Exists (Exists, mkExists, runExists)
import Data.Newtype (class Newtype, un)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))

import Prelude as Prelude

-- | Helper for existential quantification.
data DensityF f a b = DensityF (f b -> a) (f b)

-- | Density comonad.
newtype Density f a = Density (Exists (DensityF f a))

-- | Alternative `Density` constructor.
mkDensity :: ∀ f a b. (f b -> a) -> f b -> Density f a
mkDensity f x = Density <<< mkExists $ DensityF f x

derive instance newtypeDensity :: Newtype (Density f a) _

instance functorDensity :: Functor (Density f) where
  map = map

instance applyDensity :: (Apply f) => Apply (Density f) where
  apply = apply

instance applicativeDensity :: (Applicative f) => Applicative (Density f) where
  pure = pure

instance extendDensity :: Extend (Density f) where
  extend = extend

instance comonadDensity :: Comonad (Density f) where
  extract = extract

map :: ∀ f a a'. (a -> a') -> Density f a -> Density f a'
map f' = runExists map' <<< un Density
  where map' :: ∀ b. DensityF f a b -> Density f a'
        map' (DensityF f x) = mkDensity (f' <<< f) x

apply :: ∀ f a a'. Apply f => Density f (a -> a') -> Density f a -> Density f a'
apply f' x' = runExists (runExists apply' (un Density f')) (un Density x')
  where apply' :: ∀ bl br. DensityF f (a -> a') bl -> DensityF f a br -> Density f a'
        apply' (DensityF fl xl) (DensityF fr xr) =
          mkDensity (\k -> fl (fst <$> k) (fr (snd <$> k))) ((/\) <$> xl <*> xr)

pure :: ∀ f a. Applicative f => a -> Density f a
pure x = mkDensity (const x) (Prelude.pure unit)

extend :: ∀ f a a'. (Density f a -> a') -> Density f a -> Density f a'
extend f' = runExists extend' <<< un Density
  where extend' :: ∀ b. DensityF f a b -> Density f a'
        extend' (DensityF f x) = mkDensity (f' <<< mkDensity f) x

extract :: ∀ f a. Density f a -> a
extract = runExists extract' <<< un Density
  where extract' :: ∀ b. DensityF f a b -> a
        extract' (DensityF f x) = f x
