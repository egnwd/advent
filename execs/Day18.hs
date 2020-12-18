{-|
   Name: Operation Order
   Url: <https://adventofcode.com/2020/day/18>
-}

module Day18 (main) where

import Advent
import Text.Megaparsec
import Control.Monad
import Control.Monad.Combinators.Expr

main :: IO ()
main = solve table1 >> solve table2
  where solve = print . sum <=< getParsedLines 18 . expr

expr t = makeExprParser (term t) t
term t = parens (expr t) <|> number
table1 = [ [ binary "+" (+), binary "*" (*) ] ]
table2 = [ [ binary "+" (+)], [ binary "*" (*) ] ]
binary name f = InfixL (f <$ symbol name)
