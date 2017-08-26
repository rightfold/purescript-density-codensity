-- | Codensity monad.
module Control.Monad.Codensity
  ( Codensity (..)
  , unCodensity
  , liftCodensity
  , lowerCodensity
  ) where

import Prelude

import Control.Monad.Trans.Class (class MonadTrans)

-- | Codensity monad.
newtype Codensity f a = Codensity (∀ b. (a -> f b) -> f b)

-- | Unwrap a `Codensity`.
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

instance monadTransCodensity :: MonadTrans Codensity where
  lift = liftCodensity

liftCodensity :: ∀ f a. Bind f => f a -> Codensity f a
liftCodensity x = Codensity (x >>= _)

lowerCodensity :: ∀ f a. Applicative f => Codensity f a -> f a
lowerCodensity x = unCodensity x pure
