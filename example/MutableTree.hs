{-# LANGUAGE NoImplicitPrelude #-}

module MutableTree
  ( Tree (..)
  , empty
  , elem
  , insert
  , delete
  ) where

import Prelude hiding (elem, min)

import Data.STRef

import Control.Monad.ST


data Tree s a
  = Nil
  | Tree (STRef s a) (STRef s (Tree s a)) (STRef s  (Tree s a))

empty :: ST s (Tree s a)
empty = return Nil

singleton :: a -> ST s (Tree s a)
singleton x = do
  a <- newSTRef x
  u <- newSTRef =<< empty
  v <- newSTRef =<< empty
  return (Tree a u v)

elem :: Ord a => a -> STRef s (Tree s a) -> ST s Bool
elem x t = do
  t' <- readSTRef t
  case t' of
    Nil ->
      return False
    Tree y u v -> do
      y' <- readSTRef y
      case compare x y' of
        EQ ->
          return True
        LT ->
          elem x u
        GT ->
          elem x v

insert :: Ord a => a -> STRef s (Tree s a) -> ST s () -- wow
insert x ref = do
  t <- readSTRef ref
  case t of
    Nil ->
      writeSTRef ref =<< singleton x
    Tree y u v -> do
      y' <- readSTRef y
      case compare x y' of
        LT ->
          insert x u
        _ ->
          insert x v

delete :: Ord a => a -> STRef s (Tree s a) -> ST s () -- such effect
delete x t = do
  t' <- readSTRef t
  case t' of
    Nil ->
      return ()
    Tree y u v -> do
      y' <- readSTRef y
      case compare x y' of
        LT ->
          delete x u
        GT ->
          delete x v
        EQ -> do
          u' <- readSTRef u
          v' <- readSTRef v
          case u' of
            Nil ->
              writeSTRef t v'
            _ ->
              case v' of
                Nil ->
                  writeSTRef t u'
                Tree z m n -> do
                  m' <- readSTRef m
                  case m' of
                    Nil ->
                      writeSTRef t (Tree z u n)
                    _ -> do
                      min <- deleteMin z v
                      min' <- readSTRef min
                      writeSTRef y min'

deleteMin :: Ord a => STRef s a -> STRef s (Tree s a) -> ST s (STRef s a)
deleteMin def t = do
  t' <- readSTRef t
  case t' of
    Nil ->
      return def
    Tree x u v -> do
      u' <- readSTRef u
      v' <- readSTRef v
      case u' of
        Nil -> do
          writeSTRef t v'
          return x
        Tree _ deeper _ ->
          deleteMin x deeper
{-# INLINE deleteMin #-}
