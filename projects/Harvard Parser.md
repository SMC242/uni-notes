started:: false
repo:: not created

# Overview
- Create a parser for Harvard references in Haskell
- Use Parsec https://hackage.haskell.org/package/parsec
- Fields are delimited with dots
    - Possible issue: there are dots in author initials
- I would need to work out what type of citation it is before starting to parse
	- Type list:	
		 
		- 
	- Alternatively, each of the fields could be parsed with all possible variations in mind 
	- 
	- See [Reference guide](https://www.mendeley.com/guides/harvard-citation-guide/)
- Each field type's functions can be applied using `ZipList`'s `<*>` operator
	- See [Learn You A Haskell](http://learnyouahaskell.com/functors-applicative-functors-and-monoids)
```haskell
-- Outline for parsing a journal article reference
type Field = String
data FieldInfo = Authors [String]
	| Date Datetime
	| Title String
	| ArticleInfo {
		articleName :: String,
		articleVolume :: String,
		articlePageRange :: (Int, Int)
	}

fields :: String -> [Field]

parseLine :: String -> [String]
parseLine s = getZipList $ ($) <$> ZipList [authors, date, title, articleInfo] <*> ZipList (fields s)
```

# Detecting citation type
Each type of citation has a different set of fields

## List of citation types
- Books
	- Edited books
	- Book with chapter
- eBooks
- Articles
	- Journals
	- Newspapers
	- Online citations include an extra field
- Photographs
- Films
- TV programmes
- Music
- Websites

## Detection strategies
### Keywords
It might be possible to look for keywords like places

### Counting fields
- Some fields are further delimited with commas
| Citation type  | Number of fields              |
|----------------|-------------------------------|
| Book           | 4                             |
| Book (chapter) | 4                             |
| eBook          | 6                             |
| Journal        | 2 (2nd field uses commas)     |
| Newspaper      | 2 (2nd field uses commas)     |
| Online article | 3 (2nd field uses commas)     |
| Photo          | 3 (similar to online)         |
| Film           | 2 (key phrase: "Directed by") |
| TV             | 2 (both fields use commas)    |
| Website        | 2 (3 if online)               |
| Music          | 2                             |