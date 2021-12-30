-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.

module AOC.Challenge.Day07 (
    day07a
  , day07b
  ) where

import AOC.Solver ((:~>)(..))
import AOC.Common (CharParser, parseLines, countTrue)
import Text.Megaparsec
import Text.Megaparsec.Char

type IPV7 = ([String], [String])

parser :: CharParser IPV7
parser = parseSupernetSection

parseSupernetSection :: CharParser IPV7
parseSupernetSection = do
    supernetSection <- takeWhile1P Nothing (/='[')
    optional (char '[') >>= \case
        Nothing -> pure ([supernetSection], [])
        Just _ -> do
            (supernetSections, hypernetSections) <- parseHypernetSection
            pure (supernetSection : supernetSections, hypernetSections)

parseHypernetSection :: CharParser IPV7
parseHypernetSection = do
    hypernetSection <- takeWhile1P Nothing (/=']')
    optional (char ']') >>= \case
        Nothing -> pure ([], [hypernetSection])
        Just _ -> do
            (supernetSections, hypernetSections) <- parseSupernetSection
            pure (supernetSections, hypernetSection : hypernetSections)

validTLS :: IPV7 -> Bool
validTLS (supernets, hypernets) = any isAbba supernets && all (not . isAbba) hypernets
    where
        isAbba (a:b:c:d:_) | a==d && b==c && b/=a = True
        isAbba (_:rs) = isAbba rs
        isAbba [] = False

validSSL :: IPV7 -> Bool
validSSL (supernets, hypernets) = any (\aba -> any (isBab aba) hypernets) (concatMap abas supernets)
    where
        abas (a:b:c:rs) | a==c && b/=a = [a,b,a] : abas (b:c:rs)
        abas (_:rs) = abas rs
        abas [] = []

        isBab aba (a:b:c:_) | a==c && b/=a && aba == [b,a,b] = True
        isBab aba (_:rs) = isBab aba rs
        isBab _   [] = False

day07a :: [IPV7] :~> Int
day07a = MkSol
    { sParse = parseLines parser
    , sShow  = show
    , sSolve = Just . countTrue validTLS
    }

day07b :: [IPV7] :~> Int
day07b = MkSol
    { sParse = parseLines parser
    , sShow  = show
    , sSolve = Just . countTrue validSSL
    }
