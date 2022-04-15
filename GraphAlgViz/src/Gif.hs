module Gif where

import Codec.Picture.Gif
import Codec.Picture
import System.Directory
import Helper
import Data.List

-- |'attemptToCreateGif' takes a path as string and attempts to use the images in the latest imagefolder to create a gif.
attemptToCreateGif :: [Char] -> IO ()
attemptToCreateGif gifPath = do
    imageFolderDirs <- listDirectory "resultFolder/ImageFolders"
    let imageFolderDir = "resultFolder/ImageFolders/"  ++ head imageFolderDirs ++ "/"
    files           <- listDirectory imageFolderDir
    let fullPaths      = map (imageFolderDir ++) (reverse $ sort files) -- Creates all the paths to the images to be converted into a gif
    eImgs           <- mapM readBitmap (reverse fullPaths) -- converts them to (Either str DynamicImage)
    let palDelayImgs   = map ((\(x,y) -> (y, 100, x)) . palettize paletteOptions . convertRGB8 . extractImage) eImgs
    let gifWithError   = writeGifImages gifPath LoopingNever palDelayImgs -- writes the gif to a file and returns something
    extractGif gifWithError --not sure what the IO result entails

-- |'paletteOptions' is a constant that returns the PaletteOptions we use in this project
paletteOptions :: PaletteOptions
paletteOptions = PaletteOptions ( MedianMeanCut {-paletteCreationMethod-}) (True {-enableImageDithering-}) ( 100 {- paletteColorCount-})


-- |'extractImage' prints an error message if the image reading went wrong and returns a 'DynamicImage' otherwise
extractImage :: Either String DynamicImage -> DynamicImage
extractImage (Left str)  = error (str ++ "extr")
extractImage (Right img) = img


-- |'extractGif' prints an error message if the gif making went wrong and returns an empty io otherwise
extractGif :: Either String (IO ()) -> IO ()
extractGif (Left str) = error (str ++ "notsure")
extractGif (Right io) = io
