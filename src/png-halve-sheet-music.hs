import Codec.Picture
import Control.Arrow
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
import System.Environment
import System.IO

type Img = Image PixelRGB8

whitePx :: PixelRGB8
whitePx = PixelRGB8 255 255 255

isLeftWhiteHLine :: Img -> Int -> Int -> Bool
isLeftWhiteHLine img leftW y = all (== whitePx)
    [pixelAt img x y | x <- [0 .. leftW - 1]]

lineMargin :: Img -> Int -> Int
lineMargin img y = length . takeWhile (== whitePx) $
    [pixelAt img x y | x <- [0 .. imageWidth img]]

easyMedian l = sort l !! (length l `quot` 2)

-- spanFuzz 0 === span
-- spanFuzz 1 allows singleton failures in too
-- 2 for pairs also
-- etc.
spanFuzz n p xs = spanFuzzCnt 0 n p xs

spanFuzzCnt _ _ _ [] = ([], [])
spanFuzzCnt c n p l@(x:xs) = if p x
  then first (x:) $ spanFuzzCnt 0 n p xs
  else
    if c < n
      then first (x:) $ spanFuzzCnt (c + 1) n p xs
      else ([], l)

takeWhileFuzz n p = fst . spanFuzz n p

main :: IO ()
main = do
    [fname] <- getArgs
    c <- BS.readFile fname
    let Right (ImageRGB8 img) = decodePng c
        imgH = imageHeight img
        imgHHalf = imgH `quot` 2
        imgHQ1 = imgH `quot` 4
        imgHQ3 = imgH * 3 `quot` 4

        lMargin = easyMedian $ map (lineMargin img) [imgHQ1 .. imgHQ3]

        myPred = isLeftWhiteHLine img (lMargin + 20)
        Just hMid = listToMaybe . filter myPred $ concat
            [[h, imgH - h] | h <- [imgHHalf, imgHHalf - 1  .. 1]]
        h1 = last $ takeWhileFuzz 2 myPred [hMid .. imgH - 1]
        h2 = last $ takeWhileFuzz 2 myPred [hMid, hMid - 1 .. 0]
 
    (h1', h2') <- if h1 - h2 > 20 && h1 < imgHQ3 && h2 > imgHQ1
          then return (h1, h2)
          else do
            hPutStrLn stderr "Fail."
            return (imgH * 9 `quot` 16, imgH * 7 `quot` 16)

    print $ imgH - h1'
    print $ imgH - h2'
