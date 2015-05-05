{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) = error "todo"

instance (Apply f, Apply g) =>
  Apply (Compose f g) where
-- Implement the (<*>) function for an Apply instance for Compose
  -- Note:
  --      Compose f (g (a->b)) <*> Compose f (g a) = Compose f (g b)
  --      <*> :: f (a -> b) -> f a -> f b
  --      lift2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c
  (Compose f) <*> (Compose a) = Compose (lift2 (<*>) f a)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure x = Compose (pure (pure x))

instance (Bind f, Bind g) =>
  Bind (Compose f g) where
-- Implement the (=<<) function for a Bind instance for Compose
  f =<< x = do
    a <- x
    f a
