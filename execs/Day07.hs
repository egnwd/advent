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

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Set as S

main :: IO ()
main = do
  input <- getParsedLines 7 parseInput
  let graph = grev $ buildGraph input
  print $ part1 graph
  print $ part2 graph

type Color = T.Text
type Count = Int

type RawInput = [(Color, [(Color, Count)])]
type Input  = Gr Color Count
type Output = Int

ident = T.pack <$> manyTill (L.charLiteral) (try parseBag)

parseBag = (try (symbol " bags") <|> symbol " bag")

-- | Parsing
parseInput :: Parser (Color, [(Color, Count)])
parseInput = do
  color <- ident
  symbol "contain"
  holds <- catMaybes <$> ((Nothing <$ symbol "no other bags") <|> (Just <$> parseHolds)) `sepBy` (symbol ",") <* char '.'
  let es = (uncurry (,)) <$> holds

  return (color, es)

parseHolds :: Parser (Color, Count)
parseHolds = do
  ct <- fromIntegral <$> number
  cl <- ident
  return (cl, ct)

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

part1 :: Input -> Output
part1 g = let start   = fromJust $ ufold (\ctx s -> s <|> (if lab' ctx == "shiny gold" then Just $ node' ctx else Nothing)) Nothing g
              parents = fix (S.unions . S.map (\x -> S.fromList $ x : suc g x)) (S.singleton start)
           in (length parents) - 1

fix f x
  | x == fx = fx
  | otherwise = fix f fx
  where fx = f x

part2 :: Input -> Output
part2 = const 0
