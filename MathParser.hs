module MathParser where

import Parser
import MathAST

{-
A parser that matches the grammar

expr ::= factor '+' expr      BinOp <factor> PlusOp  <expr>
      |  factor '-' expr      BinOp <factor> MinusOp <expr>
      |  factor '*' expr      BinOp <factor> TimesOp <expr>
      |  factor '/' expr      BinOp <factor> DivOp   <expr>
      |  factor               <factor>

factor ::= number             Num <number>
        |  '(' expr ')'       <expr>
-}


expr :: Parser Exp
expr =   (factor <+> (sym '+' <-+> expr)               >>=: \ (f, e) -> BinOp f PlusOp e)
     <|> (factor <+> (sym '-' <-+> expr)               >>=: \ (f, e) -> BinOp f MinusOp e)
     <|> (factor <+> (sym '*' <-+> expr)               >>=: \ (f, e) -> BinOp f TimesOp e)
     <|> (factor <+> (sym '/' <-+> expr)               >>=: \ (f, e) -> BinOp f DivOp   e)
     <|> factor

factor :: Parser Exp
factor =  (number                                      >>=: \ s -> Num (read s))
      <|> (sym '(' <-+> expr <+-> sym ')')

