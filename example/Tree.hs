{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}

module Tree
  ( Tree (..)
  , empty
  , elem
  , insert
  , delete
  , flatten
  , rotateLeft
  ) where

import Data.Monoid
import Prelude hiding (elem)

data Tree a
  = Nil
  | Tree a (Tree a) (Tree a)
  deriving (Eq, Show)


empty :: Tree a
empty = Nil

elem :: Eq a => a -> Tree a -> Bool
elem _ Nil =
  False
elem x (Tree y u v)
  | x == y =
    True
  | otherwise =
    elem x u || elem x v

insert :: Ord a => a -> Tree a -> Tree a
insert x Nil =
  Tree x Nil Nil
insert x (Tree y u v)
  | x < y =
    Tree y (insert x u) v
  | otherwise =
    Tree y u (insert x v)

delete :: Ord a => a -> Tree a -> Tree a
delete _ Nil =
  Nil
delete x (Tree y u v)
  | x < y =
    Tree y (delete x u) v
  | x > y =
    Tree y u (delete x v)
  | otherwise =
    let
      deleteMin e Nil r =
        (e, r)
      deleteMin e (Tree a b c) r
        | (m, l) <- deleteMin a b c
        = (m, Tree e l r)

      go Nil r =
        r
      go l Nil =
        l
      go l (Tree a Nil c) =
        Tree a l c
      go l (Tree a b c)
        | (z, r) <- deleteMin a b c
        = Tree z l r

    in  go u v

rotateLeft :: Tree a -> Tree a
rotateLeft Nil =
  Nil
rotateLeft t@(Tree x u v) =
  case rotateLeft v of
    Nil ->
      t
    Tree y m n ->
      Tree y (Tree x u m) n


flatten :: Tree a -> [a]
flatten Nil = []
flatten (Tree x u v) = flatten u <> [x] <> flatten v
