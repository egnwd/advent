{-|
   Name: Handy Haversacks
   Url: <https://adventofcode.com/2020/day/7>
-}

module Day07 (main, ident, parseBag) where

import Advent
import Prelude hiding (unlines)

import Data.Maybe
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.PatriciaTree (Gr)
import Text.Megaparsec hiding (empty)
import Text.Megaparsec.Char
import Data.Monoid

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Set as S

main :: IO ()
main = do
  graph <- buildGraph <$> getParsedLines 7 parseInput
  let target = "shiny gold"
  print $ part1 (grev graph) target
  print $ part2 graph target

type Color = T.Text
type Count = Int

type RawInput = [(Color, [(Color, Count)])]
type Input  = Gr Color Count
type Output = Int

ident = T.pack <$> manyTill L.charLiteral (try parseBag)

parseBag = try (symbol " bags") <|> symbol " bag"

-- | Parsing
parseInput :: Parser (Color, [(Color, Count)])
parseInput = do
  color <- ident
  symbol "contain"
  holds <- catMaybes <$> ((Nothing <$ symbol "no other bags") <|> (Just <$> parseHolds)) `sepBy` symbol "," <* char '.'
  let es = uncurry (,) <$> holds

  return (color, es)

parseHolds :: Parser (Color, Count)
parseHolds = flip (,) <$> number <*> ident

buildGraph :: RawInput -> Input
buildGraph adj = let (es, (_, g)) = run empty (go adj) in mkGraph (labNodes g) es
  where
    go :: RawInput -> NodeMapM Color Count Gr [LEdge Count]
    go [] = return []
    go ((cl,adj):r) = do
      insMapNodeM cl
      insMapNodesM $ map fst adj
      es <- fromJust <$> mkEdgesM (map (\(cl', ct) -> (cl, cl', ct)) adj)
      (es ++) <$> go r

part1 :: Input -> Color -> Output
part1 g target = let start   = fromJust . getAlt $ ufoldm (isTarget target) g
                     parents = fix (S.unions . S.map (\x -> S.fromList $ x : suc g x)) (S.singleton start)
                  in length parents - 1

part2 :: Input -> Color -> Output
part2 g target = let start    = fromJust . getAlt $ ufoldm (isTarget target) g
                     children = countBags (lsuc g) (start, 1)
                  in children - 1

ufoldm f = ufold (mappend . f) mempty

isTarget :: Color -> Context Color a -> Alt Maybe Node
isTarget target ctx
  | lab' ctx == target = Alt . Just . node' $ ctx
  | otherwise = Alt Nothing

countBags f (b, c) = getSum $ Sum c <> foldMap (Sum . (c *) . countBags f) (f b)
