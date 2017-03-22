{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Refine where

import Prelude

import Data.Char
import Control.Monad
import Language.Haskell.TH
import Test.QuickCheck

qRefine :: Name -> Name -> Name -> Name -> [Name] -> Name -> Q Exp
qRefine ty_a op_a ty_c op_c names_c fun = do
  let
    abstract_name i =
      "abstract_" ++ (fmap toLower . show $ ty_a) ++ "_" ++ show i --wow
    abstract ref_ty f (arg, ty) n
      | ref_ty == ty
      = valD (varP n) (normalB (appE (varE f) (varE arg))) []
      | otherwise
      = valD (varP n) (normalB (varE arg)) []
    abstract_app ns =
      foldr appE (varE op_a) (fmap varE ns)
    concrete_app ns =
      foldr appE (varE op_c) (fmap varE ns)
    nameOfType t =
      case t of
        ListT ->
          mkName "list"
        ConT n ->
          n
        _ ->
          error ("can't read name " ++ show t)

  op_a_ret <- functionReturnsT op_a
  op_c_ret <- functionReturnsT op_c

  args_a <- zip names_c . fmap (mkName . show) <$> functionT op_a

  if nameOfType op_a_ret == ty_a && nameOfType op_c_ret == ty_c
  then do
    names_a <- mapM (newName . abstract_name) [0..length names_c - 1]
    lets <- zipWithM (abstract ty_a fun) args_a names_a
    letE (fmap pure lets)
         [|$(abstract_app names_a) === $(concrete_app names_c)|]
  else
    [e|undefined|]

functionT :: Name -> Q [Type]
functionT name = do
  info <- reify name
  case info of
    VarI _ ty _ _ ->
      return . takeArrowsT $ ty
    _ ->
      error ("not a function: " ++ nameBase name)

functionReturnsT :: Name -> Q Type
functionReturnsT name = do
  info <- reify name
  traceM $ show info
  case info of
    VarI _ ty _ _ ->
      return . takeArrowT $ ty
    _ ->
      error ("not a function: " ++ nameBase name)

takeArrowT :: Type -> Type
takeArrowT (AppT ArrowT t) = takeArrowT t
takeArrowT t = t

takeArrowsT :: Type -> [Type]
takeArrowsT =
  let
    go xs (AppT ArrowT t) =
      t : xs
    go xs _ =
      xs
  in reverse . go []
