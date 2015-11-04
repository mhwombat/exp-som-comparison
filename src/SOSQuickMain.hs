------------------------------------------------------------------------
-- |
-- Module      :  SOSQuickMain
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

type ClassifierType = SOS Int Double Word16 Image

testClassifier :: ClassifierType 
testClassifier = makeSOS sosLearningFunction sosSize threshold False imageDiff makeImageSimilar

trainOne :: (Int, ClassifierType) -> (FilePath, Image) -> IO (Int, ClassifierType)
trainOne (n, c) (f, p) = do
  putHtmlLn $ "-----" ++ show n ++ "-----"
  putHtmlLn $ "presenting " ++ f ++ ' ':imageToHtml p
  let c' = train c p
  putImageGrid 10 (M.elems . modelMap $ c')
  return (n+1, c')

main :: IO ()
main = do
  imgs <- take 100 <$> readImages trainingDir
  _ <- foldM trainOne (0, testClassifier) imgs
  putHtmlLn "test complete"
