{-# LANGUAGE BangPatterns #-}

import Codec.Picture
import Codec.Picture.Types
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Function
import Data.List
import Data.Maybe
import System.Environment

type Img = Image PixelRGB8

-- Problem with one file not having perfectly white background..
isBasicallyWhite :: PixelRGB8 -> Bool
isBasicallyWhite p = p == whitePx || r >= enough && b >= enough && g >= enough
  where
    PixelRGB8 r g b = p
    enough = 250

whitePx :: PixelRGB8
whitePx = PixelRGB8 255 255 255

type Label = Int

data ContigPx = ContigPx
    { cPxSets    :: !(IM.IntMap IS.IntSet)
    , cTranTable :: !TranTable
    , cPxToLabel :: !(MutableImage RealWorld Pixel32)
    }

initP :: Img -> IO ContigPx
initP img = do
    let imgW = imageWidth img
        imgH = imageHeight img
    ContigPx IM.empty tEmpty <$> createMutableImage imgW imgH 0

offsetToPx :: ContigPx -> Int -> (Int, Int)
offsetToPx !p !i = (x, y)
  where (y, x) = i `quotRem` mutableImageWidth (cPxToLabel p)

pxToOffset :: ContigPx -> (Int, Int) -> Int
pxToOffset !p (!x, !y) = y * (mutableImageWidth (cPxToLabel p)) + x

addP :: Int -> Int -> Int -> Label -> ContigPx -> IO ContigPx
addP !x !y !offset !label !c = do
    writePixel (cPxToLabel c) x y $ fromIntegral label
    return $ c {cPxSets =
        IM.insertWith IS.union label (IS.singleton offset) (cPxSets c)}

replLabelP :: Label -> Label -> ContigPx -> ContigPx
replLabelP !lFrom !lTo !c = c
    { cPxSets = pxSets3
    , cTranTable = tInsert lFrom lTo (cTranTable c)
    }
  where
    pxSets = cPxSets c
    Just fromSet = IM.lookup lFrom pxSets
    pxSets2 = IM.delete lFrom pxSets
    pxSets3 = IM.insertWith IS.union lTo fromSet pxSets2

data TranTable = TranTable
    { tTo  :: !(IM.IntMap Int)
    , tFro :: !(IM.IntMap IS.IntSet)
    }

tEmpty = TranTable IM.empty IM.empty

tInsert :: Int -> Int -> TranTable -> TranTable
tInsert !x !y !(TranTable to fro) = TranTable to2 fro2
  where
    to2 = foldl' (\m x0 -> IM.insert x0 y m) to (IS.toList xs)
    fro2 = IM.insertWith IS.union y xs fro
    xs = IS.insert x . fromMaybe IS.empty $ IM.lookup x fro

tLookup :: TranTable -> Int -> Int
tLookup (TranTable to _) i = fromMaybe i $ IM.lookup i to

addPointP :: Int -> Int -> ContigPx -> IO ContigPx
addPointP !x !y !c = do
    let pxToLabel = cPxToLabel c
        tranTable = cTranTable c
    leftLabel <- if x == 0 then return 0
      else fromIntegral <$> readPixel pxToLabel (x - 1) y
    topLabel <- if y == 0 then return 0
      else tLookup tranTable . fromIntegral <$> readPixel pxToLabel x (y - 1)
    let offset = pxToOffset c (x, y)
    if leftLabel == 0
      then if topLabel == 0
        then addP x y offset offset c
        else addP x y offset topLabel c
      else if topLabel == 0
        then addP x y offset leftLabel c
        else addP x y offset leftLabel $ replLabelP topLabel leftLabel c

main :: IO ()
main = do
    [fname, errPrefix] <- getArgs
    c <- BS.readFile fname
    let Right (ImageRGB8 img) = decodePng c
        imgW = imageWidth img
        imgH = imageHeight img
        hMid = imgH `quot` 2
        h1 = imgH `quot` 3
        h2 = imgH - h1
        hDiff = h2 - h1
    contigPx <- initP img
    contigPx2 <- foldM (\c2 f -> f c2) contigPx [addPointP x y
        | y <- [h1 .. h2]
        , x <- [0 .. imgW - 1]
        , not . isBasicallyWhite $ pixelAt img x y
        ]
    let pxSets = IM.elems $ cPxSets contigPx2
    {-
    let colors = [PixelRGB8 (32 * r - 1) (32 * g - 1) (32 * b - 1)
            | r <- [3 .. 8]
            , g <- [3 .. 8]
            , b <- [3 .. 8]
            ]
    img2 <- createMutableImage imgW imgH (PixelRGB8 0 0 0)
    sequence_ [
        writePixel img2 x y color
        >> writePixel img2 0 y color
        | (pxSet, color) <- zip pxSets $ cycle colors
        , offset <- IS.toList pxSet
        , let (x, y) = offsetToPx contigPx2 offset
        ]
    {-
    sequence_ [
        writePixel img2 x y (PixelRGB8 color 127 127)
        | (y, color) <- zip [half1Btm, half2Top] [255, 63]
        , x <- [1, 3 .. imgW]
        ]
    -}
    img3 <- unsafeFreezeImage img2
    writePng (fname ++ ".debug.png") img3
    -}
    let exts = map fst $ sortBy (flip compare `on` snd)
            [ ((yMin, yMax), yDiff)
            | pxSet <- pxSets
            , let ys = map (snd . offsetToPx contigPx2) $ IS.toList pxSet
            , let yMin = minimum ys
            , let yMax = maximum ys
            , let yDiff = yMax - yMin
            , yDiff < hDiff
            ]
        -- Take the largest section.
        ext1:rest = exts
        -- And the next largest section not contained in the first.
        extRest = filter
            (\(yMin, yMax) -> yMin < fst ext1 || yMax > snd ext1) rest
        ext2:_ = extRest
        -- Order them vertically.
        (sect1, sect2) =
            if fst ext1 < fst ext2 then (ext1, ext2) else (ext2, ext1)
        -- Compute dividing lines.
        sectsOverlap = snd sect1 >= fst sect2
        [half1Btm, half2Top] = case exts of
          [] -> [hMid, hMid]
          _ -> case extRest of
            [] -> replicate 2 $
              minimumBy (compare `on` abs . subtract hMid) [fst ext1, snd ext1]
            _ -> if sectsOverlap
              then [snd sect1, fst sect2]
              else [fst sect2, snd sect1]
    print half1Btm
    print half2Top
