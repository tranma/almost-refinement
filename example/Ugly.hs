{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ugly where

import qualified Tree as Pure
import SavageTree (CTree (..), CNode (..))
import qualified SavageTree as C

import Data.Int
import Data.Monoid

import Foreign.Ptr
import Foreign.Storable

import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.IO.Unsafe

import Prelude hiding (elem)


instance Arbitrary (Ptr CTree) where
  arbitrary =
    let
      nil =
        unsafePerformIO C.empty -- wow
      tree = do
        t <- arbitrary
        x <- arbitrary :: Gen Int
        let _ = unsafePerformIO $ C.insert (fromIntegral x) t -- woah
        return t
    in oneof [ pure nil, tree ]
  shrink _ = []

abstract :: Ptr CTree -> IO (Pure.Tree Int)
abstract ptr = do
  t <- peekElemOff ptr 0
  let
    fromNode p = do
      node <- peekElemOff p 0
      case _tag node of
        0 ->
          return Pure.Nil
        1 ->
          Pure.Tree (fromIntegral (_wow node)) <$> fromNode (_left node) <*> fromNode (_right node)
        x ->
          error ("wow: tag = " <> show x)
  fromNode (_root t)

abstract_eq :: Pure.Tree Int -> Pure.Tree Int -> Property
abstract_eq = (===)

--------------------------------------------------------------------------------

prop_elem :: Int -> Ptr CTree -> Property
prop_elem x ctree = monadicIO $ do
  tree <- run $ abstract ctree
  b    <- run $ C.elem (fromIntegral x) ctree
  stop $ b === Pure.elem x tree

prop_insert :: Int -> Ptr CTree -> Property
prop_insert x ctree = monadicIO $ do
  !tree   <- run $ abstract ctree
  _       <- run $ C.insert (fromIntegral x) ctree
  !tree'  <- run $ abstract ctree
  let tree'' = Pure.insert x tree
  stop $ abstract_eq tree' tree''

prop_delete :: Int -> Ptr CTree -> Property
prop_delete x ctree = monadicIO $ do
  !tree   <- run $ abstract ctree
  _       <- run $ C.delete (fromIntegral x) ctree
  !tree'  <- run $ abstract ctree
  let tree'' = Pure.delete x tree
  stop $ abstract_eq tree' tree''


return []
ugly :: IO Bool
ugly = $quickCheckAll

