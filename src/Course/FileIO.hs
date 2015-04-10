{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell io.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
-- main = getArgs >>= test
main = error "todo"

test a = (void $ sequence $ run <$> a)

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run a = (getFiles $ lines a) >>= printFiles

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles a = 
  sequence $ getFile <$> a

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile a = ((,) a) <$> readFile a

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles l = void $ sequence $ printFile' <$> l
               where printFile' (a,b) = printFile a b

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile a b = putStrLn $ "==========" ++ a ++ "\n" ++ b

