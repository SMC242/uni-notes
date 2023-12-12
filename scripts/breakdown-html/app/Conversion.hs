{-
Strategy:
1. Scan through the input line-by-line
2. Check if the line might meet one of the parsers' requirements
3. If so, attempt to parse it
    3.1. If parsing is successful, append to the output stream.
    3.2. If parsing fails, append to the error stream.
4. Repeat until end of input
5. Return the output stream and the error stream
-}
{-# LANGUAGE OverloadedStrings #-}

module Conversion where

import Control.Monad.Writer.Lazy
import Data.Char (isSpace)
import qualified Data.Text as T

data OutputEntry a = Value a | Error a deriving (Show, Eq)

type OutputWriter a = WriterT [OutputEntry a] IO

isHeadingFormat :: T.Text -> Bool
isHeadingFormat line = do
  let stripped = T.dropWhile isSpace line
  case T.uncons stripped of
    Nothing -> False
    Just (c, rest) -> c == '#' && isSpace rest

checkValidFormat :: T.Text -> Bool
checkValidFormat line = case (first, second) of
  (Nothing, _) -> False
  (Just (_, _), Nothing) -> False
  (Just ('#', _), Just (' ', rest)) -> "Pros" `T.isPrefixOf` rest
  where
    stripped = T.dropWhile isSpace line
    first = T.uncons stripped
    second = first >>= \(c, rest) -> if c == '#' then T.uncons rest else Nothing

convert :: (T.Text -> T.Text) -> T.Text -> OutputWriter T.Text ()
convert check input = do
  let lines_ = T.lines input
  forM_ lines_ $ \line -> do
    let parsed = check line
    case parsed of
      Just x -> tell [Value x]
      Nothing -> tell [Error line]