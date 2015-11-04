------------------------------------------------------------------------
-- |
-- Module      :  SOSMain
-- Copyright   :  (c) Amy de Buitléir 2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Learning demo.
--
------------------------------------------------------------------------
module Main where

import Image
import Control.Monad (foldM)
import Data.Datamining.Clustering.SOS
import qualified Data.Map.Strict as M
import Data.Word(Word16)
import Util

type Label = Word16
type ClassifierType = SOS Int Double Label Image

testClassifier :: ClassifierType 
testClassifier = makeSOS sosLearningFunction sosSize threshold False imageDiff makeImageSimilar

trainOne :: (ClassifierType, [(Label,Numeral)], [(Label, Numeral)]) -> (FilePath, Image) -> IO (ClassifierType, [(Label, Numeral)], [(Label, Numeral)])
trainOne (c, modelCreationData, stats) (f, p) = do
  let c' = train c p
  let (bmu, _, _, _)  = classify c' p
  let numeral = head f
  putStrLn $ f ++ "," ++ numeral : "," ++ show bmu
  let modelCreationData' = case lookup bmu modelCreationData of
                             Just _  -> modelCreationData
                             Nothing -> (bmu, numeral):modelCreationData
  return (c', modelCreationData', (bmu, numeral):stats)

testOne :: [(Label, Numeral)] -> ClassifierType -> [(Numeral, Bool)] -> (FilePath, Image) -> IO [(Numeral, Bool)]
testOne key c stats (f, p) = do
  let (bmu, _, _, _)  = classify c p
  let numeral = head f
  let answer = safeLookup bmu key
  let correct = answer == numeral
  putStrLn $ f ++ "," ++ numeral : "," ++ show bmu ++ "," ++ show answer ++ "," ++ show correct
  return $ (numeral, correct):stats

main :: IO ()
main = do
  putStrLn "====="
  putStrLn "Training"
  putStrLn "====="
  trainingImages <- readImages trainingDir
  putStrLn "filename,numeral,label"
  (trainedClassifier, modelCreationData, stats) <- foldM trainOne (testClassifier, [], []) trainingImages
  let answers = makeAnswerKey stats
  putStrLn ""
  putStrLn "====="
  putStrLn "Models"
  putStrLn "====="
  putStrLn "<p>"
  putImageGrid 10 (M.elems . modelMap $ trainedClassifier)
  putStrLn "</p>"
  putStrLn ""
  putStrLn "====="
  putStrLn "Answer Key"
  putStrLn "====="
  mapM_ (putStrLn . show) answers
  testImages <- readImages testDir
  putStrLn ""
  putStrLn "====="
  putStrLn "Testing"
  putStrLn "====="
  putStrLn "filename,numeral,label,answer,correct"
  stats2 <- foldM (testOne answers trainedClassifier) [] testImages
  putStrLn ""
  putStrLn "====="
  putStrLn "Summary"
  putStrLn "====="
  putStrLn $ "number of models created: " ++ show (numModels trainedClassifier)
  let (numModelsChanged, fractionModelsChanged) = countModelChanges modelCreationData answers
  putStrLn $ "number of models changed: " ++ show numModelsChanged
  putStrLn $ "fraction models changed: " ++ show fractionModelsChanged
  putStrLn ""
  putStrLn "numeral,count,correct,accuracy"
  let stats3 = numeralStats stats2
  mapM_ (putStrLn . show) stats3
