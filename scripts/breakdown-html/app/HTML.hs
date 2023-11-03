module HTML where

import Data.List (intercalate)
import qualified Data.Text as T

import Markdown (Contents)

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

testDOM :: HTMLElement
testDOM =
  TagWithChildren
    (HTMLElementData "ul" "breakdown")
    [ TagWithContent (HTMLElementData "li" "pro") "Pro",
      TagWithContent (HTMLElementData "li" "con") "Con"
    ]