{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Exit (exitSuccess, exitFailure)
import Test.HUnit
import Text.Megaparsec (parse, eof, errorBundlePretty)
import qualified Markdown as M
import qualified Data.Text as T

assertParsed :: M.Parser a -> T.Text -> (a -> Assertion) -> Assertion
assertParsed parser input cont = do
  let result = parse (parser <* eof) ""  input
  case result of
    Left err -> assertFailure $ "Parse error:\n" ++ errorBundlePretty err
    Right parsed -> cont parsed

testMdHeading :: Test
testMdHeading = TestCase $
  assertParsed
    M.markdownFormat
    input $
      assertEqual
        "should find 2 pros and cons"
        expected
    
  where 
    expected = M.HeadingFormat [M.ListElement "Pro 1" M.Pro, M.ListElement "Pro 2" M.Pro, M.ListElement "Con 1" M.Con, M.ListElement "Con 2" M.Con]
    input = T.concat ["# Pros\n"
        ,"- Pro 1\n"
        ,"- Pro 2\n"
        ,"# Cons\n"
        ,"- Con 1\n"
        ,"- Con 2\n"]

testMdColon :: Test
testMdColon = TestCase $
  assertParsed
    M.markdownFormat
    input $
      assertEqual
        "should find 2 pros and cons"
        expected
  where
    input = T.concat ["Pros:\n"
      ,"- Pro 1\n"
      ,"- Pro 2\n"
      ,"Cons:\n"
      ,"- Con 1\n"
      ,"- Con 2\n"]
    expected = M.ColonFormat [M.ListElement "Pro 1" M.Pro, M.ListElement "Pro 2" M.Pro, M.ListElement "Con 1" M.Con, M.ListElement "Con 2" M.Con]

testMdInline :: Test
testMdInline = TestCase $
  assertParsed
    M.markdownFormat
    input $
      assertEqual
        "should find 2 pros and cons"
        expected
  where
    input = T.concat ["- Pro: Pro 1\n"
      ,"- Pro: Pro 2\n"
      ,"- Con: Con 1\n"
      ,"- Con: Con 2\n"]
    expected =  M.InlineFormat [M.ListElement "Pro 1" M.Pro, M.ListElement "Pro 2" M.Pro, M.ListElement "Con 1" M.Con, M.ListElement "Con 2" M.Con]

tests :: Test
tests = TestList [
  testMdHeading
  , testMdColon
  , testMdInline
  ]

main :: IO ()
main = do runTestTT tests >>= \x -> if errors x + failures x == 0 then exitSuccess else exitFailure