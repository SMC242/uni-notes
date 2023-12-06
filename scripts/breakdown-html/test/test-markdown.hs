{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Exit (exitFailure)
import Test.HUnit
import Text.Megaparsec (parse)
import qualified Markdown as M


testMdHeading :: Parser (Either e (M.Format M.ListElement))
testMdHeading = do
  let input = "# Pros\
    \- Pro 1\
    \- Pro 2\
    \# Cons\
    \- Con 1\ 
    \- Con 2"
  let expected = 
  parse M.markdownFormat "" input

assertParsedEqual :: Parser a -> String -> a -> b -> Test
assertParsedEqual parser msg expected input = case parse parser "" input of
  Left err -> fail (show err)
  Right actual -> assertEqual msg expected actual

testMdHeading' :: Test
testMdHeading' = TestCase $
  assertParsedEqual
    M.markdownFormat
    "should find 2 pros and cons"
    expected
    input
    
  where 
    expected = M.HeadingFormat [M.ListElement "Pro 1" M.Pro, M.ListElement "Pro 2" M.Pro, M.ListElement "Con 1" M.Con, M.ListElement "Con 2" M.Con]
    input = "# Pros\
    \- Pro 1\
    \- Pro 2\
    \# Cons\
    \- Con 1\ 
    \- Con 2"

testMdColon :: IO ()
testMdColon = do
    let input = "Pros:\
    \- Pro 1\
    \- Pro 2\
    \Cons:\
    \- Con 1\
    \- Con 2"
    let expected = ColonFormat [M.ListElement "Pro 1" M.Pro, M.ListElement "Pro 2" M.Pro, M.ListElement "Con 1" M.Con, M.ListElement "Con 2" M.Con]
    let actual = parse M.markdownFormat "" input
    logTest "testMdColon" expected actual

testMdInline :: IO ()
testMdInline = do
    let input = "- Pro: Pro 1\
    \- Pro: Pro 2\
    \- Con: Con 1\
    \- Con: Con 2"
    let expected = InlineFormat [M.ListElement "Pro 1" M.Pro, M.ListElement "Pro 2" M.Pro, M.ListElement "Con 1" M.Con, M.ListElement "Con 2" M.Con]
    let actual = parse M.markdownFormat "" input
    logTest "testMdInline" expected actual

tests = [
  testMdHeading
  , testMdColon
  , testMdInline
  ]

main :: IO ()
main = do
  forM_ tests id