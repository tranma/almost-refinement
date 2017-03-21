{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Refine where

import Prelude

import Language.Haskell.TH


data Refine = Refine {
    abstractType :: Type
  , abstractOp :: Name
  , concreteType :: Type
  , concreteOp :: Name
  , arguments :: [Name]
  , abstraction :: Name
  }

qRefine :: Refine -> Q Exp
qRefine (Refine ty_a op_a ty_c op_c args abstract) = do
  op_a_ret <- functionReturnsT op_a
  op_c_ret <- functionReturnsT op_c
  if op_a_ret == ty_a && op_c_ret == ty_c
  then do
    [e|
      undefined
      -- let x = abstract $(_)
      -- in $(varE op_a) 
      |]
  else
    [e|undefined|]

functionReturnsT :: Name -> Q Type
functionReturnsT name = do
  info <- reify name
  case info of
    VarI _ ty _ _ ->
      takeArrowT ty
    _ ->
      error ("not a function: " ++ nameBase name)

takeArrowT :: Type -> Q Type
takeArrowT (AppT ArrowT t) = takeArrowT t
takeArrowT t = return t
