import Codec.Picture
import qualified Data.ByteString as BS
import Data.Maybe
import System.Environment

isLeftWhiteHLine img leftW y = all (== PixelRGB8 255 255 255)
    [pixelAt img x y | x <- [0 .. leftW - 1]]

main :: IO ()
main = do
    [fname] <- getArgs
    c <- BS.readFile fname
    let Right (ImageRGB8 img) = decodePng c
        imgH = imageHeight img
        imgHHalf = imgH `quot` 2
        pred = isLeftWhiteHLine img
        Just hMid = listToMaybe . filter (pred 43) $ concat
            [[h, imgH - h] | h <- [imgHHalf, imgHHalf - 1  .. 1]]
        h1 = last $ takeWhile (pred 43) [hMid .. imgH - 1]
        h2 = last $ takeWhile (pred 43) [hMid, hMid - 1 .. 0]
    {-
    print $ isLeftWhiteHLine img hMid 50
    print $ hMid
    -}
    print $ imgH - h1
    print $ imgH - h2
