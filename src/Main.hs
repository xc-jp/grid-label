module Main where

import           Codec.Picture
import           Control.Monad
import qualified Data.ByteString                  as BS
import           Data.Maybe                       (fromJust)
import qualified Data.Set                         as S
import qualified Data.Vector.Storable             as V
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Options.Applicative
import           System.Exit
import           System.FilePath                  (takeFileName, (-<.>))

import           Zipper

type PSet = S.Set (Int,Int)
type PicZip = Zipper (PSet, FilePath, Picture)

data MouseState = Released | Selecting | Deselecting

data AppState = AppState
  { picZipper  :: PicZip
  , mouseState :: MouseState
  , picScale :: Float
  }

data Options = Options
  { files  :: [FilePath]
  , width  :: Int
  , height :: Int
  }

optparser :: Parser Options
optparser = Options
  <$> some (strArgument (metavar "FILENAME+" <> help "Input image(s)"))
  <*> option auto (help "Horizontal label resolution" <> short 'w' <> long "width")
  <*> option auto (help "Vertical label resolution" <> short 'h' <> long "height")

imgToPic :: Image PixelRGBA8 -> Picture
imgToPic (Image w h d) = bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) (BS.pack $ V.toList d) True

annotate :: Int -> Int -> Int -> Int -> [(FilePath, Picture)] -> IO ()
annotate gridW gridH imgW imgH images =
  playIO (InWindow "Annotate" (imgW,imgH) (0,0))
        black
        60
        initState
        draw
        handle
        step
  where
    gridW' = fromIntegral gridW
    gridH' = fromIntegral gridH
    imgW' = fromIntegral imgW
    imgH' = fromIntegral imgH
    dx = imgW' / gridW'
    dy = imgH' / gridH'

    imgToGrid :: Float -> (Float, Float) -> (Int, Int)
    imgToGrid s (x,y) = (floor $ (x/s + imgW'/2)/dx, floor $ (y/s + imgH'/2)/dy)

    gridToImg :: (Int, Int) -> (Float, Float)
    gridToImg (x,y) = (fromIntegral x * dx - imgW'/2, fromIntegral y * dy - imgH'/2)

    initState :: AppState
    initState = AppState (fromJust . fromList $ fmap (\(fp, img) -> (mempty,fp,img)) images) Released 1

    drawSet set = mconcat $ do
      x <- [0 .. gridW - 1]
      y <- [0 .. gridH - 1]
      let gridPos = (x,y)
      let (ox,oy) = gridToImg (x,y)
      let rect = if S.member gridPos set then rectangleSolid else rectangleWire
      pure . translate (ox + dx/2) (oy + dy/2) $ rect dx dy

    draw :: AppState -> IO Picture
    draw (AppState (Zipper _ (set, fp, pic) _) _ s) = pure $
        scale s s bg <> translate ((+12) . negate $ s*imgW'/2) ((+12) . negate $ s*imgH'/2) pathText
      where
        bg = pic <> drawSet set
        pathText = color green . scale 0.2 0.2 . text . takeFileName $ fp

    stateOp Selecting   = S.insert
    stateOp Deselecting = S.delete
    stateOp Released    = flip const

    handle :: Event -> AppState -> IO AppState
    handle (EventKey (MouseButton LeftButton) Up _ _) (AppState zipper _ s) = pure $ AppState zipper Released s
    handle (EventKey (MouseButton LeftButton) Down _ pos) (AppState (Zipper l (set, fp, pic) r) _ s) = pure $
      let pos' = imgToGrid s pos
          mstate' = (if S.member pos' set then Deselecting else Selecting)
       in AppState (Zipper l (stateOp mstate' pos' set, fp, pic) r) mstate' s

    handle (EventMotion pos) (AppState (Zipper l (set, fp, pic) r) mstate s) = pure $
      AppState (Zipper l (stateOp mstate (imgToGrid s pos) set, fp, pic) r) mstate s

    handle (EventKey key Down _ _) (AppState zipper _ s) | key `elem` leftKeys  = pure $ AppState (goLeft zipper) Released s
    handle (EventKey key Down _ _) (AppState zipper _ s) | key `elem` rightKeys = pure $ AppState (goRight zipper) Released s
    handle (EventKey key Down _ _) (AppState zipper _ _) | key `elem` exitKeys = do
      forM_ zipper $ \(set, fp, _) ->
        let image = setToImage set gridW gridH
         in saveBmpImage (fp -<.> "label.bmp") (ImageRGB8 image)
      exitSuccess
    handle (EventResize (winW,winH)) (AppState zipper _ _) = pure $ AppState zipper Released s
      where
        winW' = fromIntegral winW
        winH' = fromIntegral winH
        s = min (winW'/imgW') (winH'/imgH')

    handle _ l = pure l

    exitKeys  = [SpecialKey KeyEsc, Char 'q']
    leftKeys  = [SpecialKey KeyLeft,  Char 'h', Char 'j', Char 'n']
    rightKeys = [SpecialKey KeyRight, Char 'k', Char 'l', Char 'p']

    step :: Float -> AppState -> IO AppState
    step _ = pure

setToImage :: S.Set (Int, Int) -> Int -> Int -> Image PixelRGB8
setToImage set w h = generateImage f w h
  where
    f x y = if S.member (x,h-y-1) set then whitePixel else blackPixel
    whitePixel = PixelRGB8 255 255 255
    blackPixel = PixelRGB8 0 0 0

main :: IO ()
main = do
  Options fps w h <- execParser (info (optparser <**> helper) fullDesc)
  imgs <- forM fps $ \fp -> do
    eImg <- readImage fp
    case convertRGBA8 <$> eImg of
      Left err  -> die err
      Right img -> pure  img
  let x@(imgW,imgH):xs = (\img -> (imageWidth img, imageHeight img)) <$> imgs
  unless (all (== x) xs) $ die "Images of differing sizes"
  annotate w h imgW imgH (zip fps $ fmap imgToPic imgs)
