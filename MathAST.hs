{-|
Module       : MathAST
Description  : Abstract syntax for the Math language
-}

module MathAST where

-- | An expression in our language
data Exp = Num Double               -- ^ a literal number
         | BinOp Exp Op Exp         -- ^ a binary operation
    deriving (Show, Eq)

-- | The binary operators in our language
data Op = PlusOp    -- ^ addition
        | MinusOp   -- ^ subtraction
        | TimesOp   -- ^ multiplication
        | DivOp     -- ^ division
    deriving (Show, Eq)
