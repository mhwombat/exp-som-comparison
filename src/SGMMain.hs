------------------------------------------------------------------------
-- |
-- Module      :  SGMMain
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
import Data.Datamining.Clustering.SGM
import qualified Data.Map.Strict as M
import Data.Word(Word16)
import System.Environment (getArgs)
import Util

type Label = Word16
type ClassifierType = SGM Int Double Label Image

testClassifier :: Double -> Double -> Double -> ClassifierType 
testClassifier threshold r0 rf = makeSGM (sgmLearningFunction r0 rf) maxSGMSize threshold False imageDiff makeImageSimilar

trainOne :: (ClassifierType, [(Label,Numeral)], [(Label, Numeral)]) -> (FilePath, Image) -> IO (ClassifierType, [(Label, Numeral)], [(Label, Numeral)])
trainOne (c, modelCreationData, stats) (f, p) = do
  let (bmu, bmuDiff, _, c') = trainAndClassify c p
  let numeral = head f
  putStrLn $ f ++ "," ++ numeral : "," ++ show bmu ++ "," ++ show bmuDiff
  let modelCreationData' = case lookup bmu modelCreationData of
                             Just _  -> modelCreationData
                             Nothing -> (bmu, numeral):modelCreationData
  -- putImageGrid 10 (M.elems . modelMap $ c')
  return (c', modelCreationData', (bmu, numeral):stats)

testOne :: [(Label, Numeral)] -> ClassifierType -> [(Numeral, Bool)] -> (FilePath, Image) -> IO [(Numeral, Bool)]
testOne key c stats (f, p) = do
  let (bmu, bmuDiff, _)  = classify c p
  let numeral = head f
  let answer = safeLookup bmu key
  let correct = answer == numeral
  putStrLn $ f ++ "," ++ numeral : "," ++ show bmu ++ "," ++ show bmuDiff ++ "," ++ show answer ++ "," ++ show correct
  return $ (numeral, correct):stats

main :: IO ()
main = do
  args <- getArgs
  let trainingDir = head args
  let testDir = args !! 1
  let threshold = read $ args !! 2
  let r0 = read $ args !! 3
  let rf = read $ args !! 4
  putStrLn $ "trainingDir=" ++ trainingDir
  putStrLn $ "testDir=" ++ testDir
  putStrLn "====="
  putStrLn "Training"
  putStrLn "====="
  trainingImages <- readImages trainingDir
  putStrLn "filename,numeral,label,diff"
  (trainedClassifier, modelCreationData, stats) <- foldM trainOne (testClassifier threshold r0 rf, [], []) trainingImages
  let answers = makeAnswerKey stats
  putStrLn ""
  putStrLn "====="
  putStrLn "Models"
  putStrLn "====="
  putStrLn "<p>"
  putImageGrid 10 (M.elems . modelMap $ trainedClassifier)
  let trainedClassifier' = trainedClassifier { maxSize=0 }
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
  putStrLn "filename,numeral,label,diff,answer,correct"
  stats2 <- foldM (testOne answers trainedClassifier') [] testImages
  putStrLn ""
  putStrLn "====="
  putStrLn "Summary"
  putStrLn "====="
  putStrLn $ "threshold=" ++ show threshold
  putStrLn $ "r0=" ++ show r0
  putStrLn $ "rf=" ++ show rf
  putStrLn $ "number of models created: " ++ show (numModels trainedClassifier)
  let (numModelsChanged, fractionModelsChanged) = countModelChanges modelCreationData answers
  putStrLn $ "number of models changed: " ++ show numModelsChanged
  putStrLn $ "fraction models changed: " ++ show fractionModelsChanged
  putStrLn ""
  putStrLn "numeral,count,correct,accuracy"
  let stats3 = numeralStats stats2
  mapM_ (putStrLn . show) stats3
