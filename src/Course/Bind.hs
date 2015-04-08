{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Bind(
  Bind(..)
, (>>=)
, join
, (<=<)
) where

import Course.Core
import Course.Functor
import Course.Apply(Apply)
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

-- | All instances of the `Bind` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. g =<< (f =<< x) ≅ ((g =<<) . f) =<< x`
class Apply f => Bind f where
  -- Pronounced, bind.
  (=<<) ::
    (a -> f b)
    -> f a
    -> f b

infixr 1 =<<

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> Id (+10) <*> Id 8
-- Id 18
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
--
-- >>> ((+) <*> (+10)) 3
-- 16
--
-- >>> ((+) <*> (+5)) 3
-- 11
--
-- >>> ((+) <*> (+5)) 1
-- 7
--
-- >>> ((*) <*> (+10)) 3
-- 39
--
-- >>> ((*) <*> (+2)) 3
-- 15

-- Note:
--      Amazing! How to use <$> (fmap) and =<< (bind) to define <*> (apply)?
--      What we want: <*> :: f (a -> b) -> f a -> f b
--      What we have: <$> :: (a -> b) -> f a -> f b
--                    =<< :: (a -> f b) -> f a -> f b
--
--      So if we have:
--                   (a -> b) -> f b
--      and:
--                   f (a -> b)
--      Then we can get:
--                   f b
--      Using =<<.
--      
--
--      For f (a -> b), it's =<<'s first parameter.
--      For (a -> b) -> f b, we can get this by <$> and =<<'s second parameter (f a). 
--      Let's use x to stand for functor (f a).
--
--      As:
--                <$> :: (a -> b) -> f a -> f b
--      then:
--                <$> x :: (a -> b) -> f b
--
--      Finally:
--               f <*> x = (<$> x) =<< f
-- 
--      One point had confused me a lot is, (eq1) is different with (eq2)!
--               <$> x :: (a -> b) -> f b        (eq1)
--               (<$>) x :: f a -> f b           (eq2)
--      In eq1, <$> without (), it's a infix operator and only given the second parameter. 
--      It's the same as fmap useing ``: 
--               `P.fmap` x :: (a -> b) -> f b 
--      While in eq2, (<$>) is a function, following by its first parameter x, and x should be a function (a -> b). 
--      It's the same as fmap without ``:
---               fmap x :: f a -> f b
(<*>) ::
  Bind f =>
  f (a -> b)
  -> f a
  -> f b
f <*> a = (<$> a) =<< f

infixl 4 <*>

-- | Binds a function on the Id monad.
--
-- >>> (\x -> Id(x+1)) =<< Id 2
-- Id 3
instance Bind Id where
  (=<<) ::
    (a -> Id b)
    -> Id a
    -> Id b
  (=<<) f (Id a) = f a

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Bind List where
  (=<<) ::
    (a -> List b)
    -> List a
    -> List b
  (=<<) f x = flatMap f x

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Bind Optional where
  (=<<) ::
    (a -> Optional b)
    -> Optional a
    -> Optional b
  (=<<) f (Full x) = f x
  (=<<) _ Empty = Empty

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance Bind ((->) t) where
  (=<<) ::
    (a -> ((->) t b))
    -> ((->) t a)
    -> ((->) t b)
  (=<<) g h = f
    where f t = g (h t) t

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join ::
  Bind f =>
  f (f a)
  -> f a
join =
  error "todo"

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
--
-- >>> ((+10) >>= (*)) 7
-- 119
(>>=) ::
  Bind f =>
  f a
  -> (a -> f b)
  -> f b
(>>=) =
  error "todo"

infixl 1 >>=

-- | Implement composition within the @Bind@ environment.
-- Pronounced, kleisli composition.
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
(<=<) ::
  Bind f =>
  (b -> f c)
  -> (a -> f b)
  -> a
  -> f c
(<=<) =
  error "todo"

infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Bind IO where
  (=<<) =
    (P.=<<)

instance Bind [] where
  (=<<) =
    (P.=<<)

instance Bind P.Maybe where
  (=<<) =
    (P.=<<)
