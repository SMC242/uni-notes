{-# LANGUAGE OverloadedStrings #-}
module Html where

import Data.List (intercalate)
import qualified Data.Text as T
import Text.Printf (printf)
import Data.List (intercalate)
import Data.Char (toLower)

import Markdown (Contents, ListElement (..), ProCon)

data HTMLElementData = HTMLElementData
  { htmlElementTagName :: Contents,
    htmElementClassName :: Contents
  } deriving Show

data HTMLElement = TagWithContent HTMLElementData Contents | TagWithChildren HTMLElementData [HTMLElement] deriving Show

joinWith :: String -> [String] -> String
joinWith = intercalate

openingTag :: HTMLElementData -> String
openingTag (HTMLElementData tagName className) = printf "<%s%s>" tagName classSegment
  where
    classSegment :: String
    classSegment = if not (T.null className) then
      printf " class=\"%s\" " (T.unpack className)
      else ""
closingTag :: HTMLElementData -> String
closingTag (HTMLElementData tagName _) = printf "</%s>" tagName

renderTag :: HTMLElementData -> String -> String
renderTag data_ innerContent = openingTag data_
  ++ innerContent
  ++ closingTag data_

indent :: Int -> String
indent = flip replicate '\t'

indentLines :: Int -> String -> String
indentLines n = unlines . map (indent n ++) . lines 

renderHTMLElement :: HTMLElement -> String
renderHTMLElement ele = aux 0 ele
  where
    aux indents e =  case e of
      TagWithContent data_ cs -> indentLines indents (renderTag data_ $ "\n"
        ++ indent (indents + 1)  -- Always indent inner content
        ++ T.unpack cs
        ++ show indents
        ++ "\n")
      TagWithChildren data_ children -> indentLines indents (renderTag data_ $
        "\n"
        ++ joinWith "\n" (map (aux (indents + 1)) children)
        ++ "\n")

   

tagWithContent :: Contents -> Contents -> Contents -> HTMLElement
tagWithContent tagName className content = TagWithContent (HTMLElementData tagName className) content

li :: Contents -> Contents -> HTMLElement
li = tagWithContent "li"

tagWithChildren :: Contents -> Contents -> [HTMLElement] -> HTMLElement
tagWithChildren tagName className children = TagWithChildren (HTMLElementData tagName className) children

ul :: Contents -> [HTMLElement] -> HTMLElement
ul = tagWithChildren "ul"

proLi = li "pro"
conLi = li "con"
breakdownUl = ul "breakdown"

toBreakdownHTML :: [ListElement] -> HTMLElement
toBreakdownHTML xs = breakdownUl $
  map
    (\(ListElement c t) -> li (T.pack . map toLower . show $ t) c)
    xs
