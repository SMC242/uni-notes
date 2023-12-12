module Html
  ( HTMLElement (..),
    HTMLElementData (..),
    renderHTMLElement,
    tagWithContent,
    tagWithChildren,
    li,
    ul,
    proLi,
    conLi,
    toBreakdownHTML,
    renderTag,
    _openingTag,
    _closingTag,
    _indentLines,
  )
where

import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Text as T
import Markdown (Contents, ListElement (..), ProCon)
import Text.Printf (printf)

data HTMLElementData = HTMLElementData
  { htmlElementTagName :: Contents,
    htmElementClassName :: Contents
  }
  deriving (Show)

data HTMLElement = TagWithContent HTMLElementData Contents | TagWithChildren HTMLElementData [HTMLElement] deriving (Show)

joinWith :: String -> [String] -> String
joinWith = intercalate

_openingTag :: HTMLElementData -> String
_openingTag (HTMLElementData tagName className) = printf "<%s%s>" tagName classSegment
  where
    classSegment :: String
    classSegment =
      if not (T.null className)
        then printf " class=\"%s\" " (T.unpack className)
        else ""

_closingTag :: HTMLElementData -> String
_closingTag (HTMLElementData tagName _) = printf "</%s>" tagName

renderTag :: HTMLElementData -> String -> String
renderTag data_ innerContent =
  _openingTag data_
    ++ innerContent
    ++ _closingTag data_

indent :: Int -> String
indent = flip replicate '\t'

_indentLines :: Int -> String -> String
_indentLines n = unlines . map (indent n ++) . lines

renderHTMLElement :: HTMLElement -> String
renderHTMLElement = aux 0
  where
    aux indents e = case e of
      TagWithContent data_ cs ->
        _indentLines
          indents
          ( renderTag data_ $
              "\n"
                ++ "\t" -- Always indent inner content
                ++ T.unpack cs
                ++ "\n"
          )
      -- !Bug: children nested > 2 times are over-indented
      TagWithChildren data_ children ->
        _indentLines
          indents
          ( renderTag data_ $
              "\n"
                ++ concatMap (aux (indents + 1)) children
          )

tagWithContent :: Contents -> Contents -> Contents -> HTMLElement
tagWithContent tagName className = TagWithContent (HTMLElementData tagName className)

li :: Contents -> Contents -> HTMLElement
li = tagWithContent "li"

tagWithChildren :: Contents -> Contents -> [HTMLElement] -> HTMLElement
tagWithChildren tagName className = TagWithChildren (HTMLElementData tagName className)

ul :: Contents -> [HTMLElement] -> HTMLElement
ul = tagWithChildren "ul"

proLi = li "pro"

conLi = li "con"

breakdownUl = ul "breakdown"

toBreakdownHTML :: [ListElement] -> HTMLElement
toBreakdownHTML xs =
  breakdownUl $
    map
      (\(ListElement c t) -> li (T.pack . map toLower . show $ t) c)
      xs
