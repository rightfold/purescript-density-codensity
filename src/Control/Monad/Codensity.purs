-- | Codensity monad.
module Control.Monad.Codensity
  ( Codensity (..)
  ) where

import Prelude

newtype Codensity f a = Codensity (∀ b. (a -> f b) -> f b)

unCodensity :: ∀ f a b. Codensity f a -> (a -> f b) -> f b
unCodensity (Codensity f) = f

instance functorCodensity :: Functor (Codensity f) where
  map f (Codensity g) = Codensity (\k -> g (k <<< f))

instance applyCodensity :: Apply (Codensity f) where
  apply (Codensity f) (Codensity g) = Codensity (\k -> f (\l -> g (k <<< l)))

instance applicativeCodensity :: Applicative (Codensity f) where
  pure x = Codensity (_ $ x)

instance bindCodensity :: Bind (Codensity f) where
  bind (Codensity f) k = Codensity (\g -> f (\x -> unCodensity (k x) g))

instance monadCodensity :: Monad (Codensity f)
