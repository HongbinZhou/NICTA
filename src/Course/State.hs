{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Implement the `Functor` instance for `State s`.
-- >>> runState ((+1) <$> pure 0) 0
-- (1,0)
instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b

  -- >>> way 1
  f <$> sa = do
    a <- sa
    return (f a)

  -- >>> way 2
  -- f <$> (State g)  = State $ \s ->
  --   let (a, newState) = g s
  --       b = f a
  --   in (b, newState)


-- | Implement the `Apply` instance for `State s`.
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])

instance Apply (State s) where
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b 

  -- way 1
  sf <*> sa = do
    f <- sf
    a <- sa
    return (f a)

  -- way 2
  -- (State f) <*> (State g) = State $ \s -> 
  --   let (f', newState) = f s
  --       (a, newState') = g newState
  --   in (f' a, newState')


-- | Implement the `Applicative` instance for `State s`.
-- >>> runState (pure 2) 0
-- (2,0)
instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure a = State $ \s -> (a, s)

-- | Implement the `Bind` instance for `State s`.
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
instance Bind (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  -- Note: ref: http://learnyouahaskell.com/for-a-few-monads-more
  f =<< (State g) = State $ \s -> let (a, s') = g s
                                      (State h) = f a
                                  in h s'

instance Monad (State s) where

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec (State f) s = let (a, s') = f s in s'

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) -> eval (State f) s == fst (runState (State f) s)
eval :: State s a -> s -> a
eval (State f) s = let (a, s') = f s in a

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get :: State s s
-- way 1:
get = State $ (\s -> (s, s))

-- way 2:
-- get = let f s = (s, s) in State f

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put :: s -> State s ()
put s = State $ (\_ -> ((), s))

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil = pure Empty
findM f (x:.xs) = (\s -> if s==True then pure (Full x) else findM f xs) =<< f x


isInSet :: Ord a => a -> State (S.Set a) Bool
isInSet a = State $ (\s -> if S.member a s then (True, s) else (False, S.insert a s))

test_isInSet :: (Bool, S.Set Integer)
test_isInSet = runState (isInSet 2) (S.fromList [1,2,3])

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat :: Ord a => List a -> Optional a
firstRepeat x = eval (findM isInSet x) S.empty

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> firstRepeat (distinct xs) == Empty
--
-- prop> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct :: Ord a => List a -> List a
distinct x = listh . S.toList $ exec (findM isInSet x) S.empty

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `findM` with `State` and `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True

-- Example:
-- sumOfSqrt(7) = 49, 
-- sumOfSqrt(4)+sumOfSqrt(9)=97, 
-- sumOfSqrt(9)+sumOfSqrt(7)=130, 
-- sumOfSqrt(1)+sumOfSqrt(3)+sumOfSqrt(0)=10
-- sumOfSqrt(1)+sumOfSqrt(0)=1, bingo!

-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy :: Integer -> Bool
isHappy n = case (identicalSeq n) of 
             (1:._) -> True
             _ -> False

nextNum :: State (List Integer) t -> State (List Integer) (Optional Integer)
nextNum (State h) = State $ (\s -> let (_, l@(x:.xs)) = h s; 
                                       v = sumOfSqrt x
                                         in if v `elem` l 
                                            then (Full x, l)
                                            else (Empty, v:.l))

sumOfSqrt :: Integer -> Integer
sumOfSqrt n = foldLeft (\acc x -> acc + (P.fromIntegral ((digitToInt x) * (digitToInt x)))) 0 (listh (show n))

seqNum :: List (State (List Integer) (Optional Integer))
seqNum = produce nextNum (pure Empty)

identicalSeq :: Integer -> List Integer
identicalSeq n = snd $ runState (findM isFull seqNum) (n:.Nil)
-- eg: identicalSeq 3, is: [16,4,20,42,145,89,58,37,61,65,81,9,3]
--     identicalSeq 7, is: [1,10,130,97,49,7], this is the happy one!


-- Test if (State (List Integer) (Optional Integer))'s value is (Full _)
isFull :: (State (List Integer) (Optional Integer)) -> (State (List Integer) Bool)
isFull (State h) = State $ (\s -> let (a, l) = h s in 
                              case a of
                               Full _ -> (True, l)
                               _ -> (False, l))

