{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SavageTree where

import Data.Int
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Prelude hiding (elem)


foreign import ccall "tree_empty" empty :: IO (Ptr CTree)
foreign import ccall "tree_elem" elem :: CInt -> Ptr CTree -> IO Bool
foreign import ccall "tree_insert" insert :: CInt -> Ptr CTree -> IO ()
foreign import ccall "tree_delete" delete :: CInt -> Ptr CTree -> IO ()

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
