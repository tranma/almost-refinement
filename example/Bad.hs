{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bad where

import qualified Tree as Pure
import qualified MutableTree as Mutable

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.ST
import Data.STRef
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Prelude


-- The region is captured in the type of mutable tree, so return an ST computation
-- to be run inside the property. Wow so awkward. Plug in Jack here:
-- https://github.com/ambiata/disorder.hs/tree/master/disorder-jack
--
genTree :: (Ord a) => Gen a -> Gen (ST s (STRef s (Mutable.Tree s a)))
genTree gen =
  oneof [
      return (newSTRef =<< Mutable.empty)
    , do x <- gen
         f <- genTree gen
         return (f >>= (\t -> Mutable.insert x t >> return t))
    ]

abstract :: STRef s (Mutable.Tree s a) -> ST s (Pure.Tree a)
abstract t = do
  t' <- readSTRef t
  case t' of
    Mutable.Nil ->
      return Pure.empty
    Mutable.Tree x u v -> do
      u' <- abstract u
      v' <- abstract v
      x' <- readSTRef x
      return (Pure.Tree x' u' v')

abstract_eq :: (Eq a, Show a) => Pure.Tree a -> Pure.Tree a -> Property
abstract_eq = (===)

--------------------------------------------------------------------------------

prop_elem :: (Arbitrary a, Ord a) => a -> Property
prop_elem x = monadicST $ forAllM (genTree arbitrary) $ \st -> do
  m  <- lift st
  m' <- run $ abstract m
  b  <- run $ Mutable.elem x m
  stop $ b === Pure.elem x m'

prop_insert :: (Arbitrary a, Ord a, Show a) => a -> Property
prop_insert x = monadicST $ forAllM (genTree arbitrary) $ \st -> do
  m  <- lift st
  p  <- run $ abstract m
  _  <- run $ Mutable.insert x m -- wow
  p' <- run $ abstract m
  let p'' = Pure.insert x p
  stop $ abstract_eq p' p''

prop_delete :: (Arbitrary a, Ord a, Show a) => a -> Property
prop_delete x = monadicST $ forAllM (genTree arbitrary) $ \st -> do
  m  <- lift st
  p  <- run $ abstract m
  _  <- run $ Mutable.delete x m -- wow
  p' <- run $ abstract m
  let p'' = Pure.delete x p
  stop $ abstract_eq p' p''

return []
bad :: IO Bool
bad = $quickCheckAll
