{-# LANGUAGE OverloadedStrings #-}
module HTML where

import Data.List (intercalate)
import qualified Data.Text as T
import Text.Printf (printf)

import Markdown (Contents, ListElement, ProCon)

data HTMLElementData = HTMLElementData
  { htmlElementTagName :: Contents,
    htmElementClassName :: Contents
  }

data HTMLElement = TagWithContent HTMLElementData Contents | TagWithChildren HTMLElementData [HTMLElement]

joinWith :: String -> [String] -> String
joinWith = intercalate

renderHTMLElement :: HTMLElement -> String
renderHTMLElement ele = case ele of
  TagWithContent data_ cs -> openingTag data_ ++ "\t" ++ T.unpack cs ++ closingTag data_
  TagWithChildren data_ children -> openingTag data_ ++ joinWith "\n" (map renderHTMLElement children) ++ closingTag data_
  where
    openingTag :: HTMLElementData -> String
    openingTag (HTMLElementData tagName className) = printf "<%s class=\"%s\" >\n" tagName className
    closingTag :: HTMLElementData -> String
    closingTag (HTMLElementData tagName _) = printf "\n</%s>" tagName

tagWithContent :: String -> String -> HTMLElement
tagWithContent tagName className content = TagWithContent (HTMLElementData tagName className) content

li :: String -> String -> HTMLElement
li = tagWithContent "li"

tagWithChildren :: String -> String -> [HTMLElement] -> HTMLElement
tagWithChildren tagName className children = TagWithChildren (HTMLElementData tagName className) children

ul :: String -> [HTMLElement] -> HTMLElement
ul = tagWithChildren "ul"

proLi = li "pro"
conLi = li "con"
breakdownUl = ul "breakdown"

toBreakdownHTML :: [ListElement] -> HTMLElement
toBreakdownHTML xs = breakdownUl $
  map
    (\(ListElement c t) -> li (toLower(show t)) c)
    xs

testDOM :: HTMLElement
testDOM =
  TagWithChildren
    (HTMLElementData "ul" "breakdown")
    [ TagWithContent (HTMLElementData "li" "pro") "Pro",
      TagWithContent (HTMLElementData "li" "con") "Con"
    ]