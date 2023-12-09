module Cli
  ( optionParser,
    Options (..),
    runCli,
  )
where

import qualified Data.Text as T
import Options.Applicative

data Options = Options
  { optionsInput :: T.Text,
    optionsInputPath :: Maybe FilePath,
    optionsOutput :: Maybe FilePath,
    optionsInPlace :: Bool
  }
  deriving (Show)

optionParser :: Parser Options
optionParser =
  Options
    <$> strOption
      ( long "input"
          <> short 'i'
          <> metavar "INPUT"
          <> help "Input file from STDIN"
      )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "OUTPUT"
              <> help "Output file path"
          )
      )
    <*> optional
      ( strOption
          ( long "input-path"
              <> short 'I'
              <> metavar "INPUT-PATH"
              <> help "Input file path. Required if `input` is not set"
          )
      )
    <*> switch
      ( long "in-place"
          <> short 'p'
          <> help "Overwrite input file. Only valid if -I is also passed"
      )

runCli :: IO Options
runCli = execParser withHelp
  where
    withHelp =
      info
        (optionParser <**> helper)
        ( fullDesc
            <> progDesc "Converts markdown pro-con lists to custom HTML"
            <> header "markdown-to-html - a markdown to HTML converter"
        )