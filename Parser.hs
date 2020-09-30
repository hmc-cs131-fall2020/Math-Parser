module Parser where

import Data.Char


------------------------------------------------------------------------------------------

type ParsingFunction = String -> (Bool, String)

-- | Runs a parser on an input and returns true if and only if the parser succeeds on the
--   entire string
parse :: ParsingFunction -> String -> Bool
parse p input = 
  case p input of 
    (True, "") -> True
    (True, remainder) -> error ("Unexpected input: " ++ remainder)
    (False, _) -> error "Parse error"

------------------------------------------------------------------------------------------

-- | A parser that always succeeds without consuming input
psucceed :: ParsingFunction
psucceed s = (True, s)

-- | A parser that always fails without consuming input
pfail :: ParsingFunction
pfail s = (False, s)

-- | A parser combinator for alternatives
(<||>) :: ParsingFunction -> ParsingFunction -> ParsingFunction
(p1 <||> p2) s = 
  case p1 s of
    (True, s') -> (True, s')
    (False, _) -> p2 s
    
-- | A parser combinator for sequencing two parsers
(<&&>) :: ParsingFunction -> ParsingFunction -> ParsingFunction
(p1 <&&> p2) s =
  case p1 s of
    (True, s') -> p2 s'
    (False, _) -> (False, s)

-- | Constructs a parser that matches a specific character
getCharThat :: (Char -> Bool) -> ParsingFunction
getCharThat _ "" = (False, "")
getCharThat cond s@(c:cs) = 
  if cond c 
    then (True, cs)
    else (False, s) 

------------------------------------------------------------------------------------------

-- | A parser combinator that parses zero or more instances of p
many :: ParsingFunction -> ParsingFunction
many p = (p <&&> many p) <||> psucceed

-- | A parser combinator that parses one or more instances of p
some :: ParsingFunction -> ParsingFunction
some p = p <&&> many p

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
skipws p = many space <&&> p

-- | A parser that matches a number (ignoring leading whitespace)
number :: ParsingFunction
number = skipws digits

-- | Constructs a parser that matches a given character (ignoring leading whitespace)
sym :: Char -> ParsingFunction
sym c = skipws (getCharThat (== c))
