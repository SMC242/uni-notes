{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Html as H
import qualified Markdown as MD
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

testOpeningTagNoClass :: Test
testOpeningTagNoClass =
  TestCase $
    assertEqual
      "Opening tag with no class"
      "<div>"
      (H.openingTag $ H.HTMLElementData "div" "")

testOpeningTagWithClass :: Test
testOpeningTagWithClass =
  TestCase $
    assertEqual
      "Opening tag with class"
      "<div class=\"foo\" >"
      (H.openingTag $ H.HTMLElementData "div" "foo")

testClosingTag :: Test
testClosingTag =
  TestCase $
    assertEqual
      "Closing tag"
      "</div>"
      (H.closingTag $ H.HTMLElementData "div" "")

testRenderTagNoClass :: Test
testRenderTagNoClass =
  TestCase $
    assertEqual
      "Render tag with no class"
      "<div>foo</div>"
      (H.renderTag (H.HTMLElementData "div" "") "foo")

testRenderTagWithClass :: Test
testRenderTagWithClass =
  TestCase $
    assertEqual
      "Render tag with class"
      "<div class=\"foo\" >foo</div>"
      (H.renderTag (H.HTMLElementData "div" "foo") "foo")

testIndentLines :: Test
testIndentLines =
  TestCase $
    assertEqual
      "Indent lines"
      "\t\tfoo\n\t\tbar\n\t\tbaz\n"
      (H.indentLines 2 "foo\nbar\nbaz\n")

testIndentLinesZero :: Test
testIndentLinesZero =
  TestCase $
    assertEqual
      "Indent lines where indents = 0"
      "foo\nbar\nbaz\n"
      (H.indentLines 0 "foo\nbar\nbaz\n")

testRenderHTMLElementTagWithContent :: Test
testRenderHTMLElementTagWithContent =
  TestCase $
    assertEqual
      "Render HTML element tag with content"
      "<div>\n\tfoo\n</div>\n"
      (H.renderHTMLElement $ H.TagWithContent (H.HTMLElementData "div" "") "foo")

testRenderHTMLElementTagWithChildren :: Test
testRenderHTMLElementTagWithChildren =
  TestCase $
    assertEqual
      "Render HTML element tag with children"
      "<div>\n\t<div>\n\t\tfoo\n\t</div>\n\t<div>\n\t\tbar\n\t</div>\n</div>\n"
      ( H.renderHTMLElement $
          H.TagWithChildren
            (H.HTMLElementData "div" "")
            [ H.TagWithContent (H.HTMLElementData "div" "") "foo",
              H.TagWithContent (H.HTMLElementData "div" "") "bar"
            ]
      )

testRenderHTMLElementNested :: Test
testRenderHTMLElementNested =
  TestCase $
    assertEqual
      "Render HTML element nested"
      "<div>\n\t<div>\n\t\tfoo\n\t</div>\n\t<div>\n\t\tbar\n\t</div>\n</div>"
      ( H.renderHTMLElement $
          H.TagWithChildren
            (H.HTMLElementData "div" "")
            [ H.TagWithChildren
                (H.HTMLElementData "div" "")
                [ H.TagWithContent (H.HTMLElementData "div" "") "foo"
                ],
              H.TagWithChildren
                (H.HTMLElementData "div" "")
                [ H.TagWithContent (H.HTMLElementData "div" "") "bar"
                ]
            ]
      )

testToBreakdownHTML :: Test
testToBreakdownHTML =
  TestCase $
    assertEqual
      "To breakdown HTML"
      "<ul class=\"breakdown\" >\n\t<li class=\"pro\" >foo</li>\n\t<li class=\"con\" >bar</li>\n</ul>"
      (H.renderHTMLElement $ H.toBreakdownHTML [MD.ListElement "foo" MD.Pro, MD.ListElement "bar" MD.Con])

tests :: Test
tests =
  TestList
    [ testOpeningTagNoClass,
      testOpeningTagWithClass,
      testClosingTag,
      testRenderTagNoClass,
      testRenderTagWithClass,
      testIndentLines,
      testRenderHTMLElementTagWithContent,
      testRenderHTMLElementTagWithChildren,
      testRenderHTMLElementNested,
      testToBreakdownHTML
    ]

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure