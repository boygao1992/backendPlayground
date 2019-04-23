{-# LANGUAGE
  BlockArguments
, DeriveDataTypeable
, DeriveFoldable
, DeriveFunctor
, DeriveGeneric
, DeriveTraversable
, ExistentialQuantification
, ExplicitForAll
, FunctionalDependencies
, FlexibleInstances
, FlexibleContexts
, GADTs
, GeneralizedNewtypeDeriving
, MultiParamTypeClasses
, NamedFieldPuns
, NoImplicitPrelude
, OverloadedStrings
, QuasiQuotes
, RankNTypes
, StandaloneDeriving
, ScopedTypeVariables
, TemplateHaskell
, TypeFamilies
#-}
module Main (main) where

import Prelude

import EsqueletoExample (example)


main :: IO ()
main = example
