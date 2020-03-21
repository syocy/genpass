{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GenPass

import Gauge
import System.IO.Silently
import System.Process (system)

main :: IO ()
main = defaultMain
  [ bench "no-compact"           $ whnfIO $ silence $ system "cabal v2-run genpass -- gen -n10000 -l4 --debug-mode=1"
  , bench "coerce-after-compact" $ whnfIO $ silence $ system "cabal v2-run genpass -- gen -n10000 -l4 --debug-mode=2"
  , bench "compact-after-coerce" $ whnfIO $ silence $ system "cabal v2-run genpass -- gen -n10000 -l4 --debug-mode=3"
  ]
