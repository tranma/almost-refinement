{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Abstraction over the structure of the type.
--
--   Refinement relation:
--
--     Tree  -- refine --> Multiset
--       |                    |
--       op                   op
--       |                    |
--       v                    v
--     Tree' -- refine --> Multiset'
--
--   Exists an abstraction function:
--
--     Tree  -- exists abstract ---> Multiset
--       |                             |
--       op                            op
--       |                             |
--       v                             v
--     Tree' -- exists abstract --> Multiset'
--
module One where

import Tree (Tree)
import qualified Tree as Tree
import qualified Data.List as List
import Test.QuickCheck
import Prelude


genTree :: Ord a => Gen a -> Gen (Tree a)
genTree gen = oneof
  [ pure Tree.empty
  , Tree.insert <$> gen <*> genTree gen ]

abstract :: Tree a -> [a]
abstract = Tree.flatten

abstract_eq :: (Show a, Ord a) => [a] -> [a] -> Property
abstract_eq xs ys = List.sort xs === List.sort ys

--------------------------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = genTree arbitrary

prop_elem_tree_list :: Eq a => a -> Tree a -> Property
prop_elem_tree_list x tree =
  let list = abstract tree
  in Tree.elem x tree === List.elem x list

prop_insert_tree_list :: (Ord a, Show a) => a -> Tree a -> Property
prop_insert_tree_list x tree =
  let list = abstract tree
      tree' = Tree.insert x tree
      list' = abstract tree'
  in abstract_eq list' (List.insert x list)

prop_delete_tree_list :: (Ord a, Show a) => a -> Tree a -> Property
prop_delete_tree_list x tree =
  let list = abstract tree
      tree' = Tree.delete x tree
      list' = abstract tree'
  in abstract_eq list' (List.delete x list)

return []
one :: IO Bool
one = $quickCheckAll
