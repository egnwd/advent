{-|
   Name: Operation Order
   Url: <https://adventofcode.com/2020/day/18>
-}

module Day18 (main) where

import Advent
import Prelude hiding (unlines)
import Text.Megaparsec
import Control.Monad.Combinators.Expr

main :: IO ()
main = do
    print . sum =<< getParsedLines 18 (expr table1)
    print . sum =<< getParsedLines 18 (expr table2)

expr t = makeExprParser (term t) t
term t = parens (expr t) <|> number
table1 = [ [ binary "+" (+), binary "*" (*) ] ]
table2 = [ [ binary "+" (+)], [ binary "*" (*) ] ]
binary  name f = InfixL  (f <$ symbol name)
