{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use unless" #-}

module Main (main) where

import Control.Monad (when)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative (
  Alternative (many),
  Parser,
  ParserResult (CompletionInvoked, Failure, Success),
  argument,
  defaultPrefs,
  execParserPure,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  optional,
  progDesc,
  renderFailure,
  str,
  strOption,
  switch,
  (<**>),
 )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Hikchr (HikchrConfig, defaultConfig, hikchrCustom)
import Hikchr qualified


data CliConfig = CliConfig
  { inputFiles :: [FilePath]
  , darkMode :: Bool
  , svgClass :: Maybe T.Text
  , continueOnError :: Bool
  }


parseArgs :: [String] -> Either String CliConfig
parseArgs args = do
  let opts =
        info
          (cliConfigParser <**> helper)
          ( fullDesc
              <> progDesc "Process Hikchr files"
              <> header "hikchr - a tool for processing Hikchr files"
          )

  case execParserPure defaultPrefs opts args of
    Success config -> Right config
    Failure failure -> Left (fst (renderFailure failure "hikchr"))
    CompletionInvoked _ -> Left "Completion invoked"


cliConfigParser :: Parser CliConfig
cliConfigParser =
  CliConfig
    <$> many (argument str (metavar "FILES..."))
    <*> switch
      ( long "dark-mode"
          <> help "Enable dark mode"
      )
    <*> optional
      ( strOption
          ( long "class"
              <> metavar "NAME"
              <> help "SVG class name"
          )
      )
    <*> switch
      ( long "dont-stop"
          <> help "Continue on error"
      )


processFile :: HikchrConfig -> Bool -> FilePath -> IO Bool
processFile config continueOnError path = do
  input <-
    if path == "-"
      then TIO.getContents
      else TIO.readFile path
  result <- hikchrCustom config input
  case result of
    Right svg -> do
      TIO.putStr svg
      return True
    Left err -> do
      when (path /= "-") $
        hPutStrLn stderr $
          "Error in file: " ++ path
      TIO.hPutStr stderr err
      return continueOnError


main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ err
      hPutStrLn stderr "Usage: hikchr [--dark-mode] [--class NAME] [--dont-stop] [FILE...]"
      exitFailure
    Right CliConfig{..} -> do
      let config =
            defaultConfig
              { Hikchr.darkMode = darkMode
              , Hikchr.svgClass = svgClass
              }
      results <- mapM (processFile config continueOnError) inputFiles
      when (not (and results)) exitFailure
