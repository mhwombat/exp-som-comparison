------------------------------------------------------------------------
-- |
-- Module      :  Util
-- Copyright   :  (c) Amy de Buitléir 2011-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functions that don't fit anywhere else.
--
------------------------------------------------------------------------
module Util where

import Control.Monad (forM_, replicateM)
import Control.Monad.Random (evalRand, mkStdGen, Rand, RandomGen, getRandomRs)
import Data.Array.ST (runSTArray)
import Data.Datamining.Clustering.SOM (decayingGaussian)
import Data.Function
import Data.List (maximumBy, group, sort, groupBy, sortBy)
import Data.List.Split (chunksOf)
import GHC.Arr (elems, listArray, readSTArray, thawSTArray, writeSTArray)
import Image
-- import Math.Geometry.Grid.Square
import System.Directory
import System.FilePath.Posix (takeFileName)

-- | From <http://www.haskell.org/haskellwiki/Random_shuffle>
shuffle :: RandomGen g => [a] -> Rand g [a]
shuffle xs = do
  let l = length xs
  rands <- take l `fmap` getRandomRs (0, l-1)
  let ar = runSTArray $ do
           ar' <- thawSTArray $ listArray (0, l-1) xs
           forM_ (zip [0..(l-1)] rands) $ \(i, j) -> do
               vi <- readSTArray ar' i
               vj <- readSTArray ar' j
               writeSTArray ar' j vi
               writeSTArray ar' i vj
           return ar'
  return (elems ar)

readDirAndShuffle :: FilePath -> IO [FilePath]
readDirAndShuffle d = do
  let g = mkStdGen 263167 -- seed
  files <- map (\f -> d ++ '/':f) . filter isImageFileName <$> getDirectoryContents d
  return $ evalRand (shuffle files) g

readImages :: FilePath -> IO [(FilePath, Image)]
readImages dir = do
  files <- readDirAndShuffle dir
  mapM readOneImage files

isImageFileName :: String -> Bool
isImageFileName s =
  s `notElem` [ "archive", ".", ".." ]

readOneImage :: FilePath -> IO (FilePath, Image)
readOneImage f = do
  img <- readImage f
  return (takeFileName f, img)

randomImages :: Int -> [Image]
randomImages n = do
  let g = mkStdGen 263167 -- seed
  evalRand (replicateM n (randomImageR 28 28 (0, 1))) g

putHtml :: String -> IO ()
putHtml s = putStr s 

putHtmlLn :: String -> IO ()
putHtmlLn s = putStrLn $ s ++ "<br/>"

putImageGrid :: Int -> [Image] -> IO ()
putImageGrid n xs = mapM_ putImageRow (chunksOf n xs)

putImageRow :: [Image] -> IO ()
putImageRow xs = mapM_ (putHtml . imageToHtml) xs >> putHtmlLn ""

imageToHtml :: Image -> String
imageToHtml x = "<img src='data:image/png;base64," ++ base64encode x ++ "'/>"

-- trainingDir :: String
-- trainingDir = "/home/amy/mnist/trainingData/"

-- testDir :: String
-- testDir = "/home/amy/mnist/testData/"

type Numeral = Char

numeralStats :: [(Numeral, Bool)] -> [(String, Int, Int, Double)]
numeralStats xs = ("all",total,totalCorrect,fraction):xs'
  where xs' = map summarise . groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ xs
        total = sum . map (\(_, x, _, _) -> x) $ xs'
        totalCorrect = sum . map (\(_, _, x, _) -> x) $ xs'
        fraction = fromIntegral totalCorrect / fromIntegral total

summarise :: [(Numeral, Bool)] -> (String, Int, Int, Double)
summarise xs = ([n], total, correct, fromIntegral correct / fromIntegral total)
  where correct = length $ filter snd xs
        total = length xs
        n = fst . head $ xs

mostCommon :: (Eq a, Ord a) => [a] -> a
mostCommon = fst . maximumBy (compare `on` snd) . map (\xs -> (head xs, length xs)) . group . sort

makeAnswerKey :: (Eq a, Ord a) => [(a, Numeral)] -> [(a, Numeral)]
makeAnswerKey = map (\(l,ns) -> (l, mostCommon ns))
                  . map (\xs -> (fst . head $ xs, map snd xs))
                  . groupBy ((==) `on` fst)
                  . sortBy (compare `on` fst)

countModelChanges :: (Eq a, Ord a) => [(a, Numeral)] -> [(a, Numeral)] -> (Int, Double)
countModelChanges xs ys = if length xs == length xs'
                            then (numChanges, fraction)
                            else error "length mismatch"
  where xs' = sortBy (compare `on` fst) xs
        ys' = sortBy (compare `on` fst) ys
        f (l1, n1) (l2, n2) | l1 /= l2   = error "label mismatch"
                            | n1 == n2  = 0
                            | otherwise = 1
        numChanges = sum $ zipWith f xs' ys'
        fraction = fromIntegral numChanges / fromIntegral (length xs)

safeLookup :: Eq a => a -> [(a, Numeral)] -> Numeral
safeLookup k vs = case lookup k vs of
                    Just v  -> v
                    Nothing -> 'X'

sgmLearningFunction :: Double -> Double -> Int -> Double
sgmLearningFunction r0 rf t = r0 * ((rf/r0)**a)
  where a = fromIntegral t / tf

somLearningFunction :: Double -> Double -> Double -> Double -> Int -> Double -> Double
somLearningFunction r0 rf w0 wf = decayingGaussian r0 rf w0 wf tf . fromIntegral

-- r0 :: Double
-- r0 = 1

-- rf :: Double
-- -- rf = 0.001
-- rf = 0.001

-- w0 :: Double
-- w0 = 3

-- wf :: Double
-- -- wf = 0.1
-- wf = 1e-15

tf :: Double
tf = 60000

-- somGrid :: RectSquareGrid
-- -- somGrid = rectSquareGrid 10 10
-- somGrid = rectSquareGrid 32 32

maxSGMSize :: Int
maxSGMSize = 1024

-- threshold :: Double
-- -- threshold = 0.16
-- threshold = 0.12

