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
      (H._openingTag $ H.HTMLElementData "div" "")

testOpeningTagWithClass :: Test
testOpeningTagWithClass =
  TestCase $
    assertEqual
      "Opening tag with class"
      "<div class=\"foo\" >"
      (H._openingTag $ H.HTMLElementData "div" "foo")

testClosingTag :: Test
testClosingTag =
  TestCase $
    assertEqual
      "Closing tag"
      "</div>"
      (H._closingTag $ H.HTMLElementData "div" "")

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
      (H._indentLines 2 "foo\nbar\nbaz\n")

testIndentLinesZero :: Test
testIndentLinesZero =
  TestCase $
    assertEqual
      "Indent lines where indents = 0"
      "foo\nbar\nbaz\n"
      (H._indentLines 0 "foo\nbar\nbaz\n")

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
      -- div>(div>div>foo)+(div>div>bar)
      "<div>\n\t<div>\n\t\t\t<div>\n\t\t\t\tfoo\n\t\t\t</div>\n\t</div>\n\t<div>\n\t\t\t<div>\n\t\t\t\tbar\n\t\t\t</div>\n\t</div>\n</div>\n"
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
      "<ul class=\"breakdown\" >\n\t<li class=\"pro\" >\n\t\tfoo\n\t</li>\n\t<li class=\"con\" >\n\t\tbar\n\t</li>\n</ul>\n"
      (H.renderHTMLElement $ H.toBreakdownHTML [MD.ListElement "foo" MD.Pro, MD.ListElement "bar" MD.Con])

testToBreakdownHTMLEmpty :: Test
testToBreakdownHTMLEmpty =
  TestCase $
    assertEqual
      "To breakdown HTML (empty input)"
      "<ul class=\"breakdown\" >\n\t\n</ul>"
      (H.renderHTMLElement $ H.toBreakdownHTML [])

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
      testToBreakdownHTML,
      testToBreakdownHTMLEmpty
    ]

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure