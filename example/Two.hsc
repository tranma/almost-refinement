{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Two where

import Data.Int
import qualified Data.List as List
import Data.Monoid

import Control.DeepSeq

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.IO.Unsafe


foreign import ccall "tree_empty" tree_empty :: IO (Ptr CTree)
foreign import ccall "tree_insert" tree_insert :: CInt -> Ptr CTree -> IO (Ptr CTree)
foreign import ccall "tree_rotate_left" tree_rotate_left :: Ptr CTree -> IO (Ptr CTree)

data CTree = CTree {
    _root :: Ptr CNode
  , _nil :: Ptr CNode
  }

instance Storable CTree where
  sizeOf _ = w * 2
  alignment = sizeOf
  peek ptr = CTree
    <$> peekWordOff ptr 0
    <*> peekWordOff ptr 1
  poke ptr (CTree a b) = do
    pokeWordOff ptr 0 a
    pokeWordOff ptr 1 b

data CNode = CNode {
    _tag :: Tag
  , _colour :: Colour
  , _wow :: Int64
  , _left :: Ptr CNode
  , _right :: Ptr CNode
  , _parent :: Ptr CNode
  } deriving (Show)

instance Storable CNode where
  sizeOf _ = w * 6
  alignment = sizeOf
  peek ptr = CNode
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 16
    <*> peekByteOff ptr 24
    <*> peekByteOff ptr 30
  poke ptr n = do
    pokeByteOff ptr 0  (_tag n)
    pokeByteOff ptr 4  (_colour n)
    pokeByteOff ptr 8  (_wow n)
    pokeByteOff ptr 16 (_left n)
    pokeByteOff ptr 24 (_right n)
    pokeByteOff ptr 30 (_parent n)

type Colour = CInt
type Tag = CInt

w :: Int
w = 8

pokeWordOff :: (Storable a) => Ptr x -> Int -> a -> IO ()
pokeWordOff ptr off x = pokeByteOff ptr (off*w) x

peekWordOff :: (Storable a) => Ptr x -> Int -> IO a
peekWordOff ptr off = peekByteOff ptr (off*w)

--------------------------------------------------------------------------------

data Tree a
  = Nil
  | Tree a (Tree a) (Tree a)
  deriving (Eq, Show)

instance NFData a => NFData (Tree a) where
  rnf Nil = ()
  rnf (Tree x a b) =
    let !_ = rnf x
        !_ = rnf a
        !_ = rnf b
    in ()

insert :: Ord a => a -> Tree a -> Tree a
insert x Nil = Tree x Nil Nil
insert x (Tree y u v)
  | x < y = Tree y (insert x u) v
  | otherwise = Tree y u (insert x v)

rotateLeft :: Tree a -> Tree a
rotateLeft Nil = Nil
rotateLeft t@(Tree x u v) =
  let v' = rotateLeft v
  in case v' of
    Nil -> t
    Tree y m n -> Tree y (Tree x u m) n

flatten :: Tree a -> [a]
flatten Nil = []
flatten (Tree x u v) = flatten u <> [x] <> flatten v

--------------------------------------------------------------------------------

instance Arbitrary (Ptr CTree) where
  arbitrary =
    let
      nil =
        unsafePerformIO tree_empty
      tree = do
        t <- arbitrary
        x <- arbitrary :: Gen Int
        return . unsafePerformIO $ tree_insert (fromIntegral x) t
    in oneof [ pure nil, tree ]
  shrink _ = []

abstract :: Ptr CTree -> IO (Tree Int)
abstract ptr = do
  t <- peekElemOff ptr 0
  let
    fromNode p = do
      node <- peekElemOff p 0
      case _tag node of
        0 ->
          return Nil
        1 ->
          Tree (fromIntegral (_wow node)) <$> fromNode (_left node) <*> fromNode (_right node)
        x ->
          error ("wow: tag = " <> show x)
  return . force =<< fromNode (_root t) -- need the force because unsafeperformIO inside gen

abstract_eq :: Tree Int -> Tree Int -> Property
abstract_eq x y =
  counterexample ("concrete: " <> show x) $
  counterexample ("abstract: " <> show y) $
    List.sort (flatten x) === List.sort (flatten y)

prop_rotate_left :: Ptr CTree -> Property
prop_rotate_left ctree = monadicIO $ do
  !tree   <- run $ abstract ctree
  !ctree' <- run $ tree_rotate_left ctree
  !tree'  <- run $ abstract ctree'
  let tree'' = rotateLeft tree
  stop $ abstract_eq tree' tree''

prop_insert :: Int -> Ptr CTree -> Property
prop_insert x ctree = monadicIO $ do
  !tree   <- run $ abstract ctree
  !ctree' <- run $ tree_insert (fromIntegral x) ctree
  !tree'  <- run $ abstract ctree'
  let tree'' = insert x tree
  stop $ abstract_eq tree' tree''

return []
two :: IO Bool
two = $quickCheckAll
