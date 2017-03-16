{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Binary tree refines sorted list.
--
module One where

import Prelude hiding (elem)
import Data.Monoid
import qualified Data.List as List
import Test.QuickCheck

data Tree a
  = Nil
  | Tree a (Tree a) (Tree a)
  deriving (Show)

elem :: Eq a => a -> Tree a -> Bool
elem _ Nil = False
elem x (Tree y u v)
  | x == y = True
  | otherwise = elem x u || elem x v

insert :: Ord a => a -> Tree a -> Tree a
insert x Nil = Tree x Nil Nil
insert x (Tree y u v)
  | x < y = Tree y (insert x u) v
  | otherwise = Tree y u (insert x v)

delete :: Ord a => a -> Tree a -> Tree a
delete _ Nil = Nil
delete x (Tree y u v)
  | x < y = Tree y (delete x u) v
  | x > y = Tree y u (delete x v)
  | otherwise =
     let
       deleteMin e Nil r =
         (e, r)
       deleteMin e (Tree a b c) r =
         let
           (m, l) = deleteMin a b c
         in (m, Tree e l r)

       go Nil r = r
       go l Nil = l
       go l (Tree a Nil c) =
         Tree a l c
       go l (Tree a b c) =
         let
           (z, r) = deleteMin a b c
         in Tree z l r

     in  go u v


flatten :: Tree a -> [a]
flatten Nil = []
flatten (Tree x u v) = flatten u <> [x] <> flatten v

--------------------------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = oneof
    [ pure Nil
    , insert <$> arbitrary <*> arbitrary ]

abstract :: Tree a -> [a]
abstract = flatten

abstract_eq :: (Show a, Ord a) => [a] -> [a] -> Property
abstract_eq xs ys = List.sort xs === List.sort ys

prop_elem_tree_list :: Eq a => a -> Tree a -> Property
prop_elem_tree_list x tree =
  let list = abstract tree
  in elem x tree === List.elem x list

prop_insert_tree_list :: (Ord a, Show a) => a -> Tree a -> Property
prop_insert_tree_list x tree =
  let list = abstract tree
      tree' = insert x tree
      list' = abstract tree'
  in abstract_eq list' (List.insert x list)

prop_delete_tree_list :: (Ord a, Show a) => a -> Tree a -> Property
prop_delete_tree_list x tree =
  let list = abstract tree
      tree' = delete x tree
      list' = abstract tree'
  in abstract_eq list' (List.delete x list)

return []
one :: IO Bool
one = $quickCheckAll
