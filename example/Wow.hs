{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Wow where

import Tree (Tree)
import qualified Tree as Tree
import Test.Refine

import qualified Data.List as List
import Test.QuickCheck
import Prelude

listElem = List.elem
treeElem = Tree.elem
listInsert = List.insert
treeInsert = Tree.insert

abstract :: Tree a -> [a]
abstract = Tree.flatten

abstract_eq :: (Show a, Ord a) => [a] -> [a] -> Property
abstract_eq xs ys = List.sort xs === List.sort ys

return []

prop_elem :: Int -> Tree Int -> Property
prop_elem x ref_tree = $(qRefineEq 'listElem 'treeElem ['x, 'ref_tree] 'abstract)

prop_insert :: Int -> Tree Int -> Property
prop_insert x ref_tree = $(qRefine 'abstract_eq 'listInsert 'treeInsert ['x, 'ref_tree] 'abstract)
