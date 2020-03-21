module GenPass.Range where

import Data.Range
import Data.List (foldl')

rangeSize :: Range Int -> Int
rangeSize (SingletonRange _) = 1
rangeSize (SpanRange (Bound x Inclusive) (Bound y Inclusive)) = y - x + 1
rangeSize (SpanRange (Bound x Inclusive) (Bound y Exclusive)) = y - x
rangeSize (SpanRange (Bound x Exclusive) (Bound y Inclusive)) = y - x
rangeSize (SpanRange (Bound x Exclusive) (Bound y Exclusive)) = y - x - 1
rangeSize _ = error "unsupported range type"

rangesSize :: [Range Int] -> Int
rangesSize = sum . map rangeSize

rangeMod :: Range Int -> Int -> Int
rangeMod (SingletonRange x) _ = x
rangeMod r@(SpanRange (Bound x Inclusive) (Bound _ Inclusive)) n =
  let s = rangeSize r in
  let m = n `mod` s in
  x + m
rangeMod _ _ = error "unsupported range type"

rangesMod :: [Range Int] -> Int -> Int
rangesMod ranges n =
  let ss = map rangeSize ranges in
  let idx = n `mod` sum ss in
  snd $ foldl' f (idx, -1) $ zip ranges ss
  where
    f (idx', -1) (SingletonRange x, _)                | idx' < 1  = (  idx',      x)
                                                      | otherwise = (idx'-1,     -1)
    f (idx', -1) (SpanRange (Bound x Inclusive) _, s) | idx' < s  = (  idx', idx'+x)
                                                      | otherwise = (idx'-s,     -1)
    f (   _, -1) _                                                = error "unsupported range type"
    f (idx',  r) _                                                = (  idx',      r)
