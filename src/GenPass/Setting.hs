{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module GenPass.Setting where

import           Barbies
import           Data.Functor.Identity
import           Data.Kind (Type)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Generics.Deriving.Monoid
import           Optics

--

data SubCommand = Gen
                | Check
                | Help
                | Version
  deriving (Generic, Eq, Ord)

instance Show SubCommand where
  show Gen     = "gen"
  show Check   = "check"
  show Help    = "help"
  show Version = "version"

readSubcommand :: Text -> Maybe SubCommand
readSubcommand "gen" = Just Gen
readSubcommand "check" = Just Check
readSubcommand "-h" = Just Help
readSubcommand "--help" = Just Help
readSubcommand "--version" = Just Version
readSubcommand _ = Nothing

--

data Setting_ (s :: SubCommand) (f :: Type -> Type) = Setting_
  { _n_ :: f Int
  , _len_ :: f Int
  , _syms_ :: f Text
  , _alphaUpper_ :: f Bool
  , _alphaLower_ :: f Bool
  , _digit_ :: f Bool
  , _hiragana_ :: f Bool
  , _katakana_ :: f Bool
  , _kind_ :: f Int
  , _verbose_ :: f Int
  , _help_ :: f Bool
  , _rest_ :: f Text
  } deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

makeLenses ''Setting_

instance Semigroup (Setting_ s Last) where
  (<>) = mappenddefault

instance Monoid (Setting_ s Last) where
  mempty = memptydefault

type Setting s = Setting_ s Identity

deriving instance AllBF Show f (Setting_ s) => Show (Setting_ s f)
deriving instance AllBF Eq f (Setting_ s) => Eq (Setting_ s f)

validate :: Setting_ s Last -> Last (Setting s)
validate = bsequence'

defaultSetting :: Setting_ s Last
defaultSetting = Setting_
  { _n_          = Last $ Just 1
  , _len_        = Last $ Just 8
  , _syms_       = Last $ Just T.empty
  , _alphaUpper_ = Last $ Just True
  , _alphaLower_ = Last $ Just True
  , _digit_      = Last $ Just True
  , _hiragana_   = Last $ Just False
  , _katakana_   = Last $ Just False
  , _kind_       = Last $ Just 0
  , _verbose_    = Last $ Just 0
  , _help_       = Last $ Just False
  , _rest_       = Last $ Just T.empty
  }

