{-# LANGUAGE OverloadedStrings, TypeFamilies  #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Solver            ((:~>)(..))
import           AOC.Common            (parseMaybeLenient, CharParser, pDecimal)
import           Control.Applicative   ((<|>))
import qualified Text.Megaparsec       as P

mulP :: CharParser Int
mulP = P.try $ (*) <$> ("mul(" *> pDecimal) <*> ("," *> pDecimal <* ")")

takeRubbish :: CharParser a -> CharParser a
takeRubbish = P.try . P.skipManyTill P.anySingle

parseGarbled :: CharParser Int -> CharParser a -> CharParser [Int]
parseGarbled p gap = go
    where
        go = takeRubbish (good <|> afterDo)
        good = (:) <$> p <*> (go <|> rubbish)
        afterDo = gap *> go
        rubbish = [] <$ P.takeRest

day03a :: [Int] :~> Int
day03a = MkSol
    { sParse = parseMaybeLenient $ parseGarbled mulP P.empty
    , sShow  = show
    , sSolve = Just . sum
    }

day03b :: [Int] :~> _
day03b = MkSol
    { sParse = parseMaybeLenient $ parseGarbled mulP ("don't()" *> takeRubbish "do()")
    , sShow  = show
    , sSolve = Just . sum
    }
