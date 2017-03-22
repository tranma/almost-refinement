{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Refine where

import Prelude

import Control.Monad
import Data.List
import Language.Haskell.TH
import Test.QuickCheck

import Debug.Trace

qRefineEq = qRefineWith Nothing
qRefine f = qRefineWith (Just f)

qRefineWith :: Maybe Name -> Name -> Name -> [Name] -> Name -> Q Exp
qRefineWith eq op_a op_c names_c abs_fun = do
  let
    abstract_name i =
      "abstract_" ++ "_" ++ show i --wow
    abstract f arg n
      | isPrefixOf "ref_" (nameBase arg)
      = valD (varP n) (normalB (appE (varE f) (varE arg))) []
      | otherwise
      = valD (varP n) (normalB (varE arg)) []
    abstract_app ns =
      foldl appE (varE op_a) (fmap varE ns)
    concrete_app ns =
      foldl appE (varE op_c) (fmap varE ns)

  names_a <- mapM (return . mkName . abstract_name) [0..length names_c - 1]
  lets <- zipWithM (abstract abs_fun) names_c names_a
  letE (fmap pure lets)
    $ case eq of
        Nothing ->
          [|$(abstract_app names_a) === $(concrete_app names_c)|]
        Just f ->
          [|$(varE f) $(abstract_app names_a) ($(varE abs_fun) $(concrete_app names_c))|]

functionT :: Name -> Q [Type]
functionT name = do
  info <- reify name
  let
    go = return . takeArrowsT
  case info of
    ClassOpI _ t _ _ ->
      go t
    VarI _ t _ _ ->
      go t
    _ ->
      error ("not a function: " ++ nameBase name)

takeArrowsT :: Type -> [Type]
takeArrowsT =
  let
    go x | trace ("go: " ++ show x) False = undefined
    go (ForallT _ _ t) =
      go t
    go (AppT ArrowT t) =
      [t]
    go (AppT x y) =
      go x ++ go y
    go _ =
      []
  in go
