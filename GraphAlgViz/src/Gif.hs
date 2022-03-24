module Gif where

import Codec.Picture.Gif
import Codec.Picture
import System.Directory


attemptToCreateGif = do
    dirs <- listDirectory "resultFolder/results1"
    let fullPaths  = map ("resultFolder/results1/" ++) dirs -- Creates all the paths to the images to be converted into a gif
    eImgs <- mapM readBitmap (reverse fullPaths) -- converts them to (Either str DynamicImage)
    let imgs = map extractImage eImgs -- Extracts the dynamic image
    let rgbImgs = map convertRGB8 imgs -- Converts to RBG8 images
    let palImgs = map (palettize paletteOptions) rgbImgs -- Converts to images with palettes and gray pixels
    let palDelayImgs = map (\(x,y) -> (y, 100, x)) palImgs -- adds gif delay and for some reason a gif with image reverses order of palette and pixels in the tuple
    let gifWithError = writeGifImages "resultFolder/gifResults/2.gif" LoopingNever palDelayImgs -- writes the gif to a file and returns something
    extractGif gifWithError --not sure what the IO result entails

-- palettize :: PaletteOptions -> Image PixelRGB8 -> (Image Pixel8, Palette)
-- convertRGB8 :: DynamicImage -> Image PixelRGB8
-- writeGifImageWithPalette :: FilePath -> Image Pixel8 -> Palette -> Either String (IO ())
-- writeGifImages :: FilePath -> GifLooping -> [(Palette, GifDelay, Image Pixel8)] -> Either String (IO ())

paletteOptions :: PaletteOptions
paletteOptions = PaletteOptions ( MedianMeanCut {-paletteCreationMethod-}) (True {-enableImageDithering-}) ( 100 {- paletteColorCount-})

extractImage :: Either String DynamicImage -> DynamicImage
extractImage (Left str) = error (str ++ "extr")
extractImage (Right img) = img

extractGif :: Either String (IO ()) -> IO ()
extractGif (Left str) = error (str ++ "notsure")
extractGif (Right io) = io


