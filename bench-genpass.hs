{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GenPass

import Gauge
import System.IO.Silently (silence)
import System.Process (system)

main :: IO ()
main = defaultMain
  [ bgroup "gen"
    [ bench "no-args"       $ whnfIO $ silence $ system "cabal v2-run genpass -- gen"
    , bench "heavy-setting" $ whnfIO $ silence $ system "cabal v2-run genpass -- gen -n1000 -l100 --hiragana --katakana --system=/-_!/"
    ]
  ]
