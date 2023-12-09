module Main where

import Cli (Options (optionsInput), runCli)
import Data.Void (Void)
import Html (toBreakdownHTML)
import Markdown (Contents, Format (..), ListElement, formatContents, markdownFormat)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, parse)

parseInput :: Options -> Either (ParseErrorBundle Contents Void) (Format ListElement)
-- TODO: support input file path
parseInput opts = parse markdownFormat "STDIN" (optionsInput opts)

main :: IO ()
main = do
  opts <- runCli
  case parseInput opts of
    Left err -> print $ errorBundlePretty err
    Right format -> print . toBreakdownHTML . formatContents $ format