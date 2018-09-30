{-# language BangPatterns #-}
module Font where

import Codec.Picture (Pixel, Image(..), DynamicImage(..), readPng, pixelAt)
import Codec.Picture.Extra (crop)
import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Monoid (Sum(..))
import Data.Semigroup ((<>))
import Data.Traversable (for)
import Data.Vector (Vector)
import Graphics.Gloss (Picture, translate, blank)
import Graphics.Gloss.Juicy (fromDynamicImage)
import System.FilePath ((</>), (<.>))

import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as Storable

data Font
  = Font
  { _fontCharWidth :: Int
  , _fontCharHeight :: Int
  , _fontCharacters :: Vector Picture
  }

data Letter
  = Letter
  { letterWidth :: Int
  , letterHeight :: Int
  , letterPicture :: Picture
  }

data Letters
  = Letters
  { lettersWidth :: Int
  , lettersHeight :: Int
  , lettersLetters :: [Letter]
  }

-- | drawn centered, like the rest of the items in gloss
drawString :: Font -> String -> Picture
drawString font = drawLetters . toLetters font

-- | drawn centered, like the rest of the items in gloss
drawChar :: Font -> Char -> Picture
drawChar font = letterPicture . getLetter font

-- | drawn centered, like the rest of the items in gloss
drawLetters :: Letters -> Picture
drawLetters letters =
  translate
    (-(fromIntegral (lettersWidth letters) / 2) + (c1w / 2))
    (fromIntegral (lettersHeight letters) / 2 - (c1h / 2)) $
  go 0 (lettersLetters letters)
  where
    (c1w, c1h) =
      case lettersLetters letters of
        [] -> (0, 0)
        h:_ -> (fromIntegral $ letterWidth h, fromIntegral $ letterHeight h)

    go !_ [] = blank
    go !n (l:ls) =
      translate n 0 (letterPicture l) <>
      go (n + fromIntegral (letterWidth l)) ls

getLetter :: Font -> Char -> Letter
getLetter font c =
  let
    oc = ord c
  in
    if 32 <= oc && oc < 127
    then
      Letter
        (_fontCharWidth font)
        (_fontCharHeight font)
        (_fontCharacters font Vector.! (oc - 32))
    else error "char out of range"

toLetters :: Font -> String -> Letters
toLetters font str =
  Letters
    (getSum $ foldMap (Sum . letterWidth) ls)
    (_fontCharHeight font)
    ls
  where
    ls = getLetter font <$> str

cropDynamicImage
  :: Int -> Int -> Int -> Int
  -> DynamicImage -> DynamicImage
cropDynamicImage x y w h di =
  case di of
    ImageY8 im -> ImageY8 $ crop x y w h im
    ImageY16 im -> ImageY16 $ crop x y w h im
    ImageYF im -> ImageYF $ crop x y w h im
    ImageYA8 im -> ImageYA8 $ crop x y w h im
    ImageYA16 im -> ImageYA16 $ crop x y w h im
    ImageRGB8 im -> ImageRGB8 $ crop x y w h im
    ImageRGB16 im -> ImageRGB16 $ crop x y w h im
    ImageRGBF im -> ImageRGBF $ crop x y w h im
    ImageRGBA8 im -> ImageRGBA8 $ crop x y w h im
    ImageRGBA16 im -> ImageRGBA16 $ crop x y w h im
    ImageYCbCr8 im -> ImageYCbCr8 $ crop x y w h im
    ImageCMYK8 im -> ImageCMYK8 $ crop x y w h im
    ImageCMYK16 im -> ImageCMYK16 $ crop x y w h im

dynamicImageWidth :: DynamicImage -> Int
dynamicImageWidth di =
  case di of
    ImageY8 im -> imageWidth im
    ImageY16 im -> imageWidth im
    ImageYF im -> imageWidth im
    ImageYA8 im -> imageWidth im
    ImageYA16 im -> imageWidth im
    ImageRGB8 im -> imageWidth im
    ImageRGB16 im -> imageWidth im
    ImageRGBF im -> imageWidth im
    ImageRGBA8 im -> imageWidth im
    ImageRGBA16 im -> imageWidth im
    ImageYCbCr8 im -> imageWidth im
    ImageCMYK8 im -> imageWidth im
    ImageCMYK16 im -> imageWidth im

-- | load a line of ascii characters from a png
-- the 0th element is space, the 126th element is tilde
loadFont
  :: FilePath
  -> Int -- ^ char width
  -> Int -- ^ char height
  -> IO Font
loadFont name cw ch = do
  res <- readPng name
  case res of
    Left err -> error $ "Couldn't load " <> name <> ". Reason:\n\n" <> err
    Right img ->
      pure . Font cw ch . Vector.fromList $
      (\x -> fromJust . fromDynamicImage $ cropDynamicImage x 0 cw ch img) <$>
      [0, cw..dynamicImageWidth img]
