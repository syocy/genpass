{-# LANGUAGE OverloadedStrings #-}

module Main where

import GenPass

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid
import System.Environment
import System.Exit
import System.IO

--

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
