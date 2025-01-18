{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (when)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Hikchr (HikchrConfig, defaultConfig, hikchrCustom)
import Hikchr qualified
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)


data CliConfig = CliConfig
  { inputFiles :: [FilePath]
  , darkMode :: Bool
  , svgClass :: Maybe T.Text
  , continueOnError :: Bool
  }


defaultCliConfig :: CliConfig
defaultCliConfig =
  CliConfig
    { inputFiles = ["-"] -- Default to stdin
    , darkMode = False
    , svgClass = Nothing
    , continueOnError = False
    }


parseArgs :: [String] -> Either String CliConfig
parseArgs = do
  let
    go config [] = Right config
    go config ("--dark-mode" : rest) =
      go config{darkMode = True} rest
    go config ("--class" : cls : rest) =
      go config{svgClass = Just (T.pack cls)} rest
    go config ("--dont-stop" : rest) =
      go config{continueOnError = True} rest
    go _config (('-' : _) : _) =
      Left
        "Unknown option. Valid options:\n\
        \--dark-mode\n\
        \--class NAME\n\
        \--dont-stop"
    go config (file : rest) =
      go
        config
          { inputFiles =
              if inputFiles config == ["-"]
                then [file]
                else inputFiles config ++ [file]
          }
        rest

  go defaultCliConfig


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
