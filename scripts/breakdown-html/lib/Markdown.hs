module Markdown
  ( Contents,
    Parser,
    Format (..),
    ProCon (..),
    ListElement (..),
    markdownFormat,
    formatContents,
  )
where

import Data.Char (toLower)
import Data.Functor (void, (<$>))
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
  deriving (Show, Eq)

data ProCon = Pro | Con deriving (Show, Eq)

data ListElement = ListElement
  { listElementContents :: Contents,
    listElementType :: ProCon
  }
  deriving (Show, Eq)

formatContents :: Format a -> [a]
formatContents (HeadingFormat xs) = xs
formatContents (ColonFormat xs) = xs
formatContents (InlineFormat xs) = xs

spaceConsumer :: Parser ()
spaceConsumer = L.space Char.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Contents -> Parser Contents
symbol = L.symbol spaceConsumer

tabLike :: Parser Contents
tabLike = T.singleton <$> (Char.char ' ' <|> Char.char '\t')

markdownListDash :: Parser ()
markdownListDash =
  optional (some tabLike) *> void (symbol "-")

markdownListElement :: Parser T.Text
markdownListElement = do
  indents <- markdownListDash
  contents <- many Char.printChar
  Char.eol
  return (T.pack contents)

markdownListElement' :: Parser (Int, T.Text)
markdownListElement' = do
  indents <- length . fromMaybe [] <$> optional (some tabLike) <* symbol "-"
  contents <- many Char.printChar
  Char.eol
  return (indents, T.pack contents)

proConList :: (Monad m) => ProCon -> m [Contents] -> m [ListElement]
proConList type_ = fmap (map (`ListElement` type_))

headingFormat :: Parser (Format ListElement)
headingFormat =
  HeadingFormat
    <$> ( header "Pros"
            *> proConList Pro (many markdownListElement)
        )
      <> (header "Cons" *> proConList Con (many markdownListElement))
  where
    header headerTitle = some (symbol "#") *> Char.string' headerTitle <* Char.eol

colonformat :: Parser (Format ListElement)
colonformat =
  ColonFormat
    <$> ( Char.string' "Pros" *> symbol ":" *> proConList Pro (many markdownListElement)
        )
      <> (Char.string' "Cons" *> symbol ":" *> proConList Con (many markdownListElement))
  where
    header headerTitle = Char.string' headerTitle <* symbol ":" <* Char.eol

inlineFormat :: Parser (Format ListElement)
inlineFormat =
  InlineFormat <$> do
    some $ do
      line <- markdownListDash
      category <-
        try
          (Pro <$ Char.string' "Pro")
          <|> (Con <$ Char.string' "Con")
      contents <- symbol ":" *> many Char.printChar <* Char.eol
      pure $ ListElement (T.pack contents) category

markdownFormat :: Parser (Format ListElement)
markdownFormat =
  choice
    [ headingFormat,
      colonformat,
      inlineFormat
    ]