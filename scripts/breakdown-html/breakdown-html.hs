{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (toLower)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Printf (printf)

type Contents = T.Text

type Parser a = Parsec Void Contents a

data HTMLElementData = HTMLElementData
  { htmlElementTagName :: Contents,
    htmElementClassName :: Contents
  }

data HTMLElement = TagWithContent HTMLElementData Contents | TagWithChildren HTMLElementData [HTMLElement]

data Format a
  = HeadingFormat [a]
  | ColonFormat [a]
  | InlineFormat [a]

data ProCon = Pro | Con

data ListElement = ListElement
  { listElementContents :: Contents,
    listElementType :: ProCon
  }

spaceConsumer :: Parser ()
spaceConsumer = L.space Char.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Contents -> Parser Contents
symbol = L.symbol spaceConsumer

tabLike :: Parser Contents
tabLike = Char.char ' ' <|> Char.char '\t'

markdownListDash :: Parser ()
markdownListDash =
  optional (many tabLike) <* symbol "-"

markdownListElement :: Parser T.Text
markdownListElement = do
  indents <- markdownListDash
  contents <- many Char.printChar
  Char.eol
  return (T.pack contents)

markdownListElement' :: Parser (Int, T.Text)
markdownListElement' = do
  indents <- length . fromMaybe <$> optional (some $ Char.char '\t') <* symbol "-"
  contents <- many Char.printChar
  Char.eol
  return (indents, T.pack contents)

markdownFormat :: Parser (Format ListElement)
markdownFormat =
  choice
    []
  where
    proConList :: (Monad m) => ProCon -> m [Contents] -> m [ListElement]
    proConList type_ = fmap (map (`ListElement` type_))
    heading :: Parser (Format ListElement)
    heading =
      HeadingFormat
        <$> ( header "Pros"
                *> proConList Pro (many markdownListElement)
            )
          <> (header "Cons" *> proConList Con (many markdownListElement))
      where
        header headerTitle = some (symbol "#") *> Char.string' headerTitle
    colon :: Parser (Format ListElement)
    colon =
      ColonFormat
        <$> ( Char.string' "Pros" *> symbol ":" *> proConList Pro (many markdownListElement)
            )
          <> (Char.string' "Cons" *> symbol ":" *> proConList Con (many markdownListElement))
      where
        header headerTitle = Char.string' headerTitle <* symbol ":" <* Char.eol
    inline :: Parser (Format ListElement)
    inline =
      InlineFormat <$> do
        many $ do
          line <- markdownListDash
          category <-
            try
              (Char.string' "Pro" $> Pro)
              <|> (Char.string' "Con" $> Con)
          contents <- symbol ":" *> many Char.printChar
          ListElement <$> contents category

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

main :: IO ()
main = undefined