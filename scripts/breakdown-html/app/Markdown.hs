{-# LANGUAGE OverloadedStrings #-}

module Markdown where

import Data.Char (toLower)
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf (printf)

type Contents = T.Text

type Parser a = Parsec Void Contents a

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
tabLike = T.pack <$> (Char.char ' ' <|> Char.char '\t')

markdownListDash :: Parser ()
markdownListDash =
  optional (some tabLike) <* symbol "-"

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
              (Pro <$ Char.string' "Pro")
              <|> (Con <$ Char.string' "Con")
          contents <- symbol ":" *> many Char.printChar
          ListElement <$> contents category
