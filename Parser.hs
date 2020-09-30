module Parser where

import Prelude hiding (return)
import Data.Char


------------------------------------------------------------------------------------------

type ParsingFunction = String -> Maybe (String, String)

-- | Runs a parser on an input and returns true if and only if the parser succeeds on the
--   entire string
parse :: ParsingFunction -> String -> String
parse p input = 
  case p input of 
    Just (result, "") -> result
    Just (_, remainder) -> error ("Unexpected input: " ++ remainder)
    Nothing -> error "Parse error"

------------------------------------------------------------------------------------------

-- | A parser that always succeeds without consuming input
return :: String -> ParsingFunction
return result s = Just (result, s)

-- | A parser that always fails without consuming input
pfail :: ParsingFunction
pfail _ = Nothing

-- | A parser combinator for alternatives
(<|>) :: ParsingFunction -> ParsingFunction -> ParsingFunction
(p1 <|> p2) s = 
  case p1 s of
    Just (result, s') -> Just (result, s')
    Nothing -> p2 s
    
-- | A parser combinator for concatenating two parsers
(<++>) :: ParsingFunction -> ParsingFunction -> ParsingFunction
(p1 <++> p2) s =
  case p1 s of
    Just (result1, s') -> case p2 s' of
                            Nothing -> Nothing
                            Just (result2, s'') -> Just (result1 ++ result2, s'')
    Nothing -> Nothing

-- | A parser combinator that discards the left result
(<-+>) :: ParsingFunction -> ParsingFunction -> ParsingFunction
(p1 <-+> p2) s =
  case p1 s of
    Just (_, s') -> p2 s'
    Nothing -> Nothing

-- | A parser combinator that discards the right result
(<+->) :: ParsingFunction -> ParsingFunction -> ParsingFunction
(p1 <+-> p2) s =
  case p1 s of
    Just (result1, s') -> case p2 s' of
                            Nothing -> Nothing
                            Just (_, s'') -> Just (result1, s'')
    Nothing -> Nothing


-- | Constructs a parser that matches a specific character
getCharThat :: (Char -> Bool) -> ParsingFunction
getCharThat _ "" = Nothing
getCharThat cond s@(c:cs) = 
  if cond c 
    then Just ([c], cs)
    else Nothing 

------------------------------------------------------------------------------------------

-- | A parser combinator that parses zero or more instances of p
many :: ParsingFunction -> ParsingFunction
many p = (p <++> many p) <|> return ""

-- | A parser combinator that parses one or more instances of p
some :: ParsingFunction -> ParsingFunction
some p = p <++> many p

-- | A parser that matches a digit
digit :: ParsingFunction
digit = getCharThat isDigit

-- | A parser that matches one or more digits
digits :: ParsingFunction
digits = some digit

-- | A parser that matches a letter
letter :: ParsingFunction
letter = getCharThat isLetter

-- | A parser that matches one or more letters
letters :: ParsingFunction
letters = some letter

-- | A parser that matches a space
space :: ParsingFunction
space = getCharThat isSpace

-- | A parser that matches one or more spaces
spaces :: ParsingFunction
spaces = some space

-- | A parser combinator that skips leading whitespace and matches p
skipws :: ParsingFunction -> ParsingFunction
skipws p = many space <-+> p

-- | A parser that matches a number (ignoring leading whitespace)
number :: ParsingFunction
number = skipws digits

-- | Constructs a parser that matches a given character (ignoring leading whitespace)
sym :: Char -> ParsingFunction
sym c = skipws (getCharThat (== c))
