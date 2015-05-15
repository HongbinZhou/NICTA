{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
anagrams s f = 
  (\c -> intersectBy equalIgnoringCase (permutations s) c)
  <$> (loadDic f)

-- dic format: 
--     file name such as: dict.dat
--     content:
--         hi
--         ih
--         ...
loadDic :: Filename -> IO (List Chars)
loadDic f = lines <$> (readFile f)

test :: Chars -> List Chars -> List Chars
test s dic = intersectBy equalIgnoringCase (permutations s) dic

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase Nil Nil = True
equalIgnoringCase _ Nil = False
equalIgnoringCase Nil _ = False
equalIgnoringCase (x:.xs) (y:.ys) = 
  (toLower x) == (toLower y) && 
  equalIgnoringCase xs ys

--  (h `on` f) :: a -> a -> c
--  h :: b -> b -> c
--  f :: a -> b
--  usually on accept two param, 
--  the first one is a function, which should apply to 'b'
--  the second one is a function too, which accept an 'a' and convert a to 'b'
--  then (h `on` f) will accept two 'a's and give the result c.
equalIgnoringCase' :: Chars -> Chars -> Bool
equalIgnoringCase' = (==) `on` (toLower <$>)
