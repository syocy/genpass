{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE StrictData, BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Barbies
import GHC.Generics
import Data.Functor.Identity
import Data.Monoid
import Generics.Deriving.Monoid
import System.Environment
import System.Exit
import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Data.Kind (Type)
import Optics
import Data.List (foldl')
import Text.Megaparsec as P
import Text.Megaparsec.Char as P
import Text.Megaparsec.Char.Lexer as P hiding (space)
import Control.Applicative
import Data.Char
import Debug.Trace
import Data.Maybe
import Paths_genpass (version)
import Data.Coerce
import Data.String.Interpolate
import Crypto.Random
import Data.Range
import Data.Bits
import Data.Word
import qualified Data.ByteArray as M
import Data.Version (showVersion)
import Control.DeepSeq
import GHC.Compact

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

--

type Parser = Parsec Text Text

parserRedundantOption :: Parser (Setting_ s Last)
parserRedundantOption = do
  _ <- char '-'
  extras <- char 'v' >> P.many (void (char 'v'))
  pure $ set verbose_ (Last $ Just (1 + length extras)) mempty

parserBatchOption :: Parser (Setting_ s Last)
parserBatchOption = do
  _ <- char '-'
  settings <- P.some (char 'h' >> pure (set help_ (Last $ Just True) mempty))
  pure $ foldl1 (<>) settings

parserEqual :: Parser ()
parserEqual = try (void (char '=')) <|> space

parserSandwitch :: Parser Text
parserSandwitch = do
  sep :: Char <- anySingle
  txt <- takeWhileP Nothing (/= sep)
  _ <- char sep
  pure txt

type SettingSetter k s v = Optic' k NoIx (Setting_ s Last) (Last v)

parserKey :: (Is k A_Setter) => Parser a -> v -> SettingSetter k s v -> Parser (Setting_ s Last)
parserKey parserKey' v field = do
  _ <- parserKey'
  pure $ set field (Last $ Just v) mempty

parserKeyValue :: (Is k A_Setter) => Parser a -> Parser v -> SettingSetter k s v -> Parser (Setting_ s Last)
parserKeyValue parserKey' parserValue field = do
  _ <- parserKey'
  parserEqual
  v <- parserValue
  pure $ set field (Last $ Just v) mempty

parserShortOption :: Parser (Setting_ s Last)
parserShortOption = do
  let parserPrefix = char '-'
  try (parserPrefix >> parserN) <|> try (parserPrefix >> parserL)
    where
      parserN = parserKeyValue (char 'n') decimal n_
      parserL = parserKeyValue (char 'l') decimal len_

parserLongOption :: Parser (Setting_ s Last)
parserLongOption = do
  let parserPrefix = string "--"
  let wrapParser p = try (parserPrefix >> p)
  foldl1 (<|>) $ map wrapParser [ parserLen, parserKind, parserSymbol
                                , parserAlphaUpper, parserNoAlphaUpper
                                , parserAlphaLower, parserNoAlphaLower
                                , parserDigit, parserNoDigit
                                , parserHiragana, parserNoHiragana
                                , parserKatakana, parserNoKatakana
                                ]
    where
      parserLen          = parserKeyValue (string "len")            decimal         len_
      parserKind         = parserKeyValue (string "kind")           decimal         kind_
      parserSymbol       = parserKeyValue (string "symbol")         parserSandwitch syms_
      parserAlphaUpper   = parserKey      (string "alpha-upper")    True            alphaUpper_
      parserNoAlphaUpper = parserKey      (string "no-alpha-upper") False           alphaUpper_
      parserAlphaLower   = parserKey      (string "alpha-lower")    True            alphaLower_
      parserNoAlphaLower = parserKey      (string "no-alpha-lower") False           alphaLower_
      parserDigit        = parserKey      (string "digit")          True            digit_
      parserNoDigit      = parserKey      (string "no-digit")       False           digit_
      parserHiragana     = parserKey      (string "hiragana")       True            hiragana_
      parserNoHiragana   = parserKey      (string "no-hiragana")    False           hiragana_
      parserKatakana     = parserKey      (string "katakana")       True            katakana_
      parserNoKatakana   = parserKey      (string "no-katakana")    False           katakana_

parserRest :: Parser (Setting_ s Last)
parserRest = do
  rest <- T.strip <$> takeRest
  pure $ set rest_ (Last (Just rest)) mempty

parserSetting :: Parser (Setting_ s Last)
parserSetting = do
  settings <- (try parserRedundantOption <|> try parserBatchOption <|> try parserShortOption <|> try parserLongOption <|> parserRest) `sepBy` (P.some spaceChar)
  pure $ foldl (<>) mempty settings

parseSetting :: Text -> Either (ParseErrorBundle Text Text) (Setting_ s Last)
parseSetting = parse parserSetting ""

--

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

composeWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
composeWord32 w1 w2 w3 w4 = w1 `shift'` 24 + w2 `shift'` 16 + w3 `shift'` 8 + w4 `shift'` 0
  where
    w `shift'` 0 = fromIntegral w
    w `shift'` n = fromIntegral w `shiftL` n

composeInts :: [Word8] -> [Int]
composeInts = reverse . composeInts' []
  where
    composeInts' result (w1:w2:w3:w4:xs) =
      let !r = fromIntegral $ composeWord32 w1 w2 w3 w4 in
        composeInts' (r:result) xs
    composeInts' result _ = result

--

runSetting :: SubCommand -> Setting s -> IO ()
runSetting Version s = runVersionSetting $ coerce s
runSetting Gen     s = (runGenSetting . getCompact) =<< compact (coerce s)
runSetting Check   s = runCheckSetting   $ coerce s
runSetting sub     _ = T.hPutStrLn stderr [i|subcommand #{sub} is not implemented|] >> exitFailure

runVersionSetting :: Setting 'Version -> IO ()
runVersionSetting _ = putStrLn $ showVersion version

runGenSetting :: Setting 'Gen -> IO ()
runGenSetting setting =
  let n   :: Int         = coerce $ view n_   setting in
  let len :: Int         = coerce $ view len_ setting in
  let rs  :: [Range Int] = makeCpSetting      setting in
  replicateM_ n $ do
    ws <- M.unpack @M.Bytes <$> getRandomBytes (4 * len)
    let xs = composeInts ws
    let str = map (chr . rangesMod rs) xs
    T.putStrLn $ T.pack str

runCheckSetting :: Setting 'Check -> IO ()
runCheckSetting setting =
  let rest :: Text        = coerce $ view rest_ setting in
  let len  :: Int         = coerce $ view len_  setting in
  let kind :: Int         = coerce $ view kind_ setting in
  let rs   :: [Range Int] = makeCpSetting       setting in
  let resultEither :: Either Text () = do
        _ <- if T.length rest >= len then Right () else Left [i|#{rest} is shorter than #{len}|]
        let xs = map ord $ T.unpack rest
        _ <- if kind <= length (filter (\r -> any (inRange r) xs)  rs) then Right () else Left [i|#{rest} contains too few kinds of chars|]
        _ <- if all (inRanges rs) xs then Right () else Left [i|#{rest} contains illegal char(s)|]
        Right ()
  in
    case resultEither of
      Left e  -> T.hPutStrLn stderr [i|#{e}|] >> exitFailure
      Right _ -> T.putStrLn [i|#{rest} is valid|] >> exitSuccess

--

instance ShowErrorComponent Text where
  showErrorComponent = T.unpack

main :: IO ()
main = do
  args <- map T.pack <$> getArgs

  let subcommandMaybe =
        if null args
          then Just Gen
          else readSubcommand (head args)

  subcommand <- case subcommandMaybe of
    Nothing -> T.hPutStrLn stderr "Need subcommand" >> exitFailure
    Just x  -> pure x

  let rest = T.intercalate " " $ case args of { [] -> []; (_:t) -> t }
  -- T.putStrLn [i|#{subcommand} #{rest}|]
  -- parseTest parserSetting rest

  let settingEither = parseSetting rest
  Last settingMaybe <- case settingEither of
                         Left e         -> T.hPutStrLn stderr (T.pack (show e)) >> exitFailure
                         Right setting_ -> pure $ validate $ defaultSetting <> setting_
  case settingMaybe of
    Nothing      -> T.hPutStrLn stderr "insufficient arguments" >> exitFailure
    Just setting -> runSetting subcommand setting

