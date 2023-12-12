{-
Strategy:
1. Scan through the input line-by-line
2. Check if the line might meet one of the parsers' requirements
3. If so, attempt to parse it.
    3.1. If parsing is successful, append to the output stream.
    3.2. If parsing fails, append to the error stream.
4. Return the output stream and the error stream.
-}
module Conversion where

import Control.Monad.Writer.Lazy
import qualified Data.Text as T

data OutputEntry a = StdOut a | StdErr a deriving (Show, Eq)

convert :: (Monad m) => (T.Text -> Maybe a) -> T.Text -> WriterT [OutputEntry a] m ()
convert parser input = do
  let lines_ = T.lines input
  forM_ lines_ $ \line -> do
    let parsed = parser line
    case parsed of
      Just x -> tell [StdOut x]
      Nothing -> tell [StdErr line]