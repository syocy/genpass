{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GenPass.Runner where

import           GenPass.Cp
import           GenPass.Range
import           GenPass.Setting
import           Paths_pkgname

import           Control.Monad
import           Crypto.Random
import           Data.Bits (shiftL)
import qualified Data.ByteArray as M
import           Data.Char
import           Data.Coerce (coerce)
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Range
import           Data.String.Interpolate
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Version (showVersion)
import           Data.Word
import           GHC.Compact
import           Optics
import           System.Exit
import           System.IO
import           Text.Megaparsec as P
import           Text.Megaparsec.Char as P
import           Text.Megaparsec.Char.Lexer as P hiding (space)

--

newtype MyParseError = MyParseError { unMyParseError :: Text }
  deriving (Show, Eq, Ord)

instance ShowErrorComponent MyParseError where
  showErrorComponent = T.unpack . coerce

--

type Parser = Parsec MyParseError Text

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

parseSetting :: Text -> Either (ParseErrorBundle Text MyParseError) (Setting_ s Last)
parseSetting = parse parserSetting ""

--

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
