------------------------------------------------------------------------
-- |
-- Module      :  SOMQuickMain
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
import Data.Datamining.Clustering.Classifier (train, models)
import Data.Datamining.Clustering.SOM
import Math.Geometry.Grid
import Math.Geometry.Grid.Square
import Math.Geometry.GridMap.Lazy
import Util

type ClassifierType = SOM Int Double (LGridMap RectSquareGrid) Double (Int, Int) Image

testClassifier :: ClassifierType
testClassifier = SOM gm somLearningFunction imageDiff makeImageSimilar 0
  where imgs = randomImages (tileCount somGrid)
        gm = lazyGridMap somGrid imgs

trainOne :: (Int, ClassifierType) -> (FilePath, Image) -> IO (Int, ClassifierType)
trainOne (n, c) (f, p) = do
  putHtmlLn $ "-----" ++ show n ++ "-----"
  putHtmlLn $ "presenting " ++ f ++ ' ':imageToHtml p
  let c' = train c p
  putImageGrid 10 (models c')
  return (n+1, c')

main :: IO ()
main = do
  imgs <- take 100 <$> readImages trainingDir
  _ <- foldM trainOne (0, testClassifier) imgs
  putHtmlLn "test complete"
