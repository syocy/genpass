{-# LANGUAGE ScopedTypeVariables #-}

module GenPass.Cp where

import           GenPass.Setting

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Range
import           Data.Char
import           Optics
import           Data.Coerce (coerce)
import           Data.Functor.Identity

cpLower, cpUpper, cpDigit, cpHira, cpKata :: Range Int
cpLower = ord 'a' +=+ ord 'z'
cpUpper = ord 'A' +=+ ord 'Z'
cpDigit = ord '0' +=+ ord '9'
cpHira  = ord 'あ' +=+ ord 'ゔ'
cpKata  = ord 'ア' +=+ ord 'ヺ'

makeCpSyms :: Text -> [Range Int]
makeCpSyms text =
  let cs :: String      = T.unpack text                  in
  let rs :: [Range Int] = map (\c -> ord c +=+ ord c) cs in
  mergeRanges rs

makeCpSetting :: Setting s -> [Range Int]
makeCpSetting setting =
  let syms       :: Text = coerce $ view syms_       setting in
  let alphaUpper :: Bool = coerce $ view alphaUpper_ setting in
  let alphaLower :: Bool = coerce $ view alphaLower_ setting in
  let digit      :: Bool = coerce $ view digit_      setting in
  let hiragana   :: Bool = coerce $ view hiragana_   setting in
  let katakana   :: Bool = coerce $ view katakana_   setting in
  let rs :: [Range Int] = mconcat [ makeCpSyms syms
                                  , if alphaUpper then [cpUpper] else mempty
                                  , if alphaLower then [cpLower] else mempty
                                  , if digit      then [cpDigit] else mempty
                                  , if hiragana   then [cpHira]  else mempty
                                  , if katakana   then [cpKata]  else mempty
                                  ] in
  mergeRanges rs
