import Codec.Picture
import qualified Data.ByteString as BS
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

main :: IO ()
main = do
    [fname] <- getArgs
    c <- BS.readFile fname
    let Right (ImageRGB8 img) = decodePng c
        imgH = imageHeight img
        imgHHalf = imgH `quot` 2
        imgHQ1 = imgH `quot` 4
        imgHQ3 = imgH * 3 `quot` 4

        lMargin = minimum $ map (lineMargin img) [imgHQ1 .. imgHQ3]

        myPred = isLeftWhiteHLine img (lMargin + 20)
        Just hMid = listToMaybe . filter myPred $ concat
            [[h, imgH - h] | h <- [imgHHalf, imgHHalf - 1  .. 1]]
        h1 = last $ takeWhile myPred [hMid .. imgH - 1]
        h2 = last $ takeWhile myPred [hMid, hMid - 1 .. 0]
 
    (h1', h2') <- if h1 - h2 > 20 && h1 < imgHQ3 && h2 > imgHQ1
          then return (h1, h2)
          else do
            hPutStrLn stderr "Fail."
            return (imgH * 9 `quot` 16, imgH * 7 `quot` 16)

    print $ imgH - h1'
    print $ imgH - h2'
