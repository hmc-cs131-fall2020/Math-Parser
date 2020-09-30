module MathParser where

import Parser

{-
A parser that matches the grammar

expr ::= factor '+' expr
      |  factor '-' expr
      |  factor '*' expr
      |  factor '/' expr
      |  factor

factor ::= number
        |  '(' expr ')'
-}

expr :: ParsingFunction
expr =  (factor <++> sym '+' <++> expr)
    <|> (factor <++> sym '-' <++> expr)
    <|> (factor <++> sym '*' <++> expr)
    <|> (factor <++> sym '/' <++> expr)        
    <|> factor

factor :: ParsingFunction
factor =  number
      <|> (sym '(' <++> expr <++> sym ')')
