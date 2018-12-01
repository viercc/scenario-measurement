{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main(main) where

import           Control.DeepSeq

import           Control.Monad.IO.Class

import qualified Data.Text              as T
import qualified Data.Text.IO           as T

import           Data.Char
import           Data.List              (sortBy)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Ord

import           Gauge
import           Scenario

targetFile,outputFile :: String
targetFile = "benchdata/in.txt"
outputFile = "benchdata/out.txt"

scenarioText :: ScenarioT IO ()
scenarioText = do
  content <- liftIO $ T.readFile targetFile
  section "ngram-all" content $ \content ->
    do content' <- section "ngram-preprocess" content (evaluateNF . preprocess)
       table <- section "ngram-table" content' (evaluateNF . makeTable)
       topRanks <- section "ngram-sort" table (evaluateNF . sortTopRanks)
       section "ngram-output" topRanks $ \topRanks ->
         liftIO $ writeFile outputFile (show topRanks)

preprocess :: T.Text -> T.Text
preprocess = T.concat . go
  where
    go cs =
      let (alphas, rest) = T.span isAlpha cs
          rest' = T.dropWhile (not . isAlpha) rest
          next = if T.null rest' then [] else space : go rest'
      in T.toLower alphas : next
    space = T.singleton ' '

makeTable :: T.Text -> Map T.Text Int
makeTable = Map.fromListWith (+) . map (\a -> (a,1)) . ngrams 4

ngrams :: Int -> T.Text -> [T.Text]
ngrams n = map (T.take n) . T.tails

sortTopRanks :: (Ord b) => Map a b -> [(a,b)]
sortTopRanks = take 30 . sortBy (comparing (Down . snd)) . Map.toList

handmadeBenchmarks :: IO [Benchmark]
handmadeBenchmarks =
  do content <- T.readFile targetFile
     let bAll = bench "ngram-all" $ whnfIO $
           do let content' = preprocess content
                  table = makeTable content'
                  topRanks = sortTopRanks table
              writeFile outputFile (show topRanks)
     let bPrep = bench "ngram-preprocess" $ preprocess `nf` content
         content' = preprocess content
     evaluate (rnf content')
     let bTable = bench "ngram-table" $ makeTable `nf` content'
         table = makeTable content'
     evaluate (rnf table)
     let bSort = bench "ngram-sort" $ sortTopRanks `nf` table
         topRanks = sortTopRanks table
     evaluate (rnf topRanks)
     let bPrint = bench "ngram-output" $ whnfIO $
           writeFile outputFile (show topRanks)
     return [bAll, bPrep, bTable, bSort, bPrint]

main :: IO ()
main =
  do ((),benchesS) <- benchScenarioT scenarioText
     benchesHand <- handmadeBenchmarks
     defaultMain
       [ bgroup "Scenario" benchesS
       , bgroup "Hand" benchesHand
       , bgroup "Overhead" (snd $ benchScenario $ treelike 16)
       ]

treelike :: Int -> Scenario ()
treelike = go 0
  where
    go lo hi | lo + 1 >= hi = return ()
             | otherwise    =
      let mid = (lo + hi) `div` 2
          label = show lo ++ "-" ++ show hi
      in section label () $ \_ -> go lo mid >> go mid hi >>= evaluateWHNF
