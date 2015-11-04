module Image where

import qualified Codec.Picture as P
import Control.Monad.Random (Rand, RandomGen, getRandoms, getRandomRs)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as UTF8
import Data.List.Split (chunksOf)
import Data.Word(Word8)
import Data.Vector.Storable (Vector, toList, fromList)

data Image = Image { iWidth :: Int, iHeight :: Int, pixels :: [Word8] }
  deriving (Eq, Show)

pixelCount :: Image -> Int
pixelCount img = iWidth img * iHeight img

mkImage :: Int -> Int -> [Word8] -> Image
mkImage w h ps = Image w h ps'
  where ps' = take (w*h) (ps ++ repeat 0)

pixelAt :: Image -> Int -> Int -> Word8
pixelAt (Image w _ ps) r c = ps !! (r*w + c)

pixelArray :: Image -> [[Word8]]
pixelArray (Image w _ ps) = chunksOf w ps

-- pretty (Image w h xs) = show w ++ "x" ++ show h ++ concatMap f xs
--   where f x = ' ' : pretty x

randomImage :: RandomGen r => Int -> Int -> Rand r Image
randomImage w h = fmap (mkImage w h . take (w*h)) getRandoms

randomImageR :: RandomGen r => Int -> Int -> (Word8, Word8) -> Rand r Image
randomImageR w h range
  = (mkImage w h . take (w * h)) <$> getRandomRs range

blankImage :: Int -> Int -> Image
blankImage w h = Image w h $ replicate (w*h) 0

indices :: Int -> Int -> [(Int, Int)]
indices w h = [(i,j) | i <- [0..h-1], j <- [0..w-1]]
  -- row, column, colour channel

readImage :: FilePath -> IO Image
readImage filePath = do
  result <- P.readImage filePath
  case result of
    Right (P.ImageY8 img) -> do
      let ps = toList $ P.imageData img
      return $ Image (P.imageWidth img) (P.imageHeight img) ps
      -- I.runIL $ fmap arrayToImage $ I.readImage filePath
    Right _ -> error $ "Wrong image format: " ++ filePath
    Left msg -> error $ "Unable to read " ++ filePath ++ ": " ++ msg

writeImage :: FilePath -> Image -> IO ()
writeImage filePath img = do
  let ps = fromList $ pixels img :: Vector (P.PixelBaseComponent P.Pixel8)
  let img' = P.Image (iWidth img) (iHeight img) ps :: P.Image P.Pixel8
  -- let dImg = P.ImageY8 img'
  P.writePng filePath img'
  -- I.runIL $ I.writeImage filePath $ imageToArray img

adjust :: [Word8] -> Double -> [Word8] -> [Word8]
adjust ts r xs
  | r < 0     = error "Negative learning rate"
  | r > 1     = error "Learning rate > 1"
  | otherwise = zipWithCopyRight (adjust' r) ts xs

adjust' :: Double -> Word8 -> Word8 -> Word8
adjust' r t x = round(x' + r*(t' - x'))
  where t' = fromIntegral t :: Double
        x' = fromIntegral x :: Double

-- Zip until we run out of elements on the left, then copy elements from
-- the right
zipWithCopyRight :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithCopyRight f (x:xs) (y:ys)
  = f x y : zipWithCopyRight f xs ys
zipWithCopyRight _ _ [] = []
zipWithCopyRight _ [] ys = ys


imageDiff :: Image -> Image -> Double
imageDiff a b
  | pixelCount a == 0 && pixelCount b == 0 = 1
  | otherwise                             = avgDelta / 255
  where xs = map fromIntegral . take l $ pixels a ++ repeat 0 :: [Double]
        ys = map fromIntegral . take l $ pixels b ++ repeat 0 :: [Double]
        l = max (pixelCount a) (pixelCount b)
        diff = sum . map abs $ zipWith (-) xs ys
        avgDelta = diff / fromIntegral l

makeImageSimilar :: Image -> Double -> Image -> Image
makeImageSimilar target amount a
    = a { pixels=adjust (pixels target) amount (pixels a) }

toPNG :: Image -> P.Image P.Pixel8
toPNG (Image w h ps) = P.Image w h (fromList ps)

base64encode :: Image -> String
base64encode
  = UTF8.toString . B64.encode . B.concat . BL.toChunks . P.encodePng
      . toPNG
