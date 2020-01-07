{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

#ifndef FIRMWARE
#define FIRMWARE "firmware"
#endif

import Codec.Picture
import Codec.Picture.Gif
import Data.List.Split
import Data.Complex
import System.Environment
import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Bits.Bitwise as BA
import Options.Applicative
import Control.Monad
import Data.Monoid
import Data.List
import Data.Bifunctor
import Data.Foldable
import System.Exit
import Data.Char
import Data.FileEmbed
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Regex.Posix
import Numeric (readHex)
import Graphics.Rasterific
import Graphics.Rasterific.Transformations hiding (Transformation)
import Graphics.Rasterific.Texture

cFRAMES = 256          :: Integer
cCENTER = 9            :: Double
cLEN    = 33+cCENTER/2 :: Double  -- Lenght of one arm
cT0RATE = 1000000      :: Integer -- TODO: Need to calibrate this

data Color = R|G|B deriving (Show, Eq)


type Offset = Double
type Shift = Double
type Rotation = Double

type BitMaker = forall a. (Color -> Bool -> Complex Double -> a) -> [a]

-- Frame (0..1) and position (-cLen..cLen) to coordinate (-1..1 × -1..1)
type Transformation = Double -> Double -> Complex Double

projectCircular :: Offset -> Shift -> Rotation -> Transformation
projectCircular off shift rot =
    \ frame pos ->
        let rho = frame*2*pi
            cpos = pos :+ 0
        in ((cpos + displacement) * cis rho * cis rot_radians) / (radius :+ 0)
  where
    displacement = shift :+ off
    rot_radians = (-rot-3)/12*2*pi
    radius = sqrt ((cLEN + abs shift)^2 + off^2)


projectLinear :: Transformation
projectLinear frame pos = (2*frame -1) :+ pos/cLEN

data TransSpec
    = Circular Offset Shift Rotation
    | Linear
    deriving Show

project :: TransSpec -> Transformation
project (Circular off shift rot) = projectCircular off shift rot
project Linear                   = projectLinear

bitBuilder :: TransSpec -> BitMaker
bitBuilder spec f =
    [ f c a cpos
    | frame <- [0,1..cFRAMES-1]
    , c <- [R,G,B]
    , (a, pos) <- ((False,) <$> arm) ++ ((True,) . negate <$> arm)
    , let cpos = trans (fromIntegral frame / fromIntegral cFRAMES) pos
    ]
  where
    trans = project spec
    arm = concatMap reverse $ chunksOf 8 $ map (cCENTER/2 +) [0..31]

getColor :: Color -> PixelRGB8 -> Bool
getColor R (PixelRGB8 r _ _ ) = r > 130
getColor G (PixelRGB8 _ g _ ) = g > 130
getColor B (PixelRGB8 _ _ b ) = b > 130

calibrationBits :: [Bool]
calibrationBits = map not $ foldr1 (zipWith (||))
    [ bitBuilder (Circular off 0 0) $ \c a z ->
        abs (phase z - off*pi/10) < pi/40 &&
       (c == oc) &&
       (if a then magnitude z > 0.75
             else magnitude z < 0.75 && (magnitude z > 0.5 || off == 0))
    | (oc, off) <- zip (cycle [R,G,B]) [-8..8] ]

dynImageToPackagedData builder dynImage =
    bitsToData $
    builder $ \col _ (x :+ y) ->
           let pix = pixelAt pixels
                  (round ((( x+1)/2) * w))
                  (round (((-y+1)/2) * h))
           in not $ getColor col pix
  where
    pixels = convertRGB8 dynImage

    w = fromIntegral (imageWidth pixels)
    h = fromIntegral (imageHeight pixels)

bitsToData = BS.pack . map BA.fromListLE . chunksOf 8


getImage :: BitMaker -> (FilePath, Double) -> IO [(BS.ByteString, Double)]
getImage builder source = case source of
        ("CALIBRATION",n) -> do
            putStrLn "Adding offset calibration image"
            pure [(bitsToData calibrationBits, n)]
        (filename, _) | takeExtension filename == ".gif" -> do
            gifData <- BS.readFile filename
            let gifImages = either error id $ decodeGifImages gifData
            let gifDelays = either error id $ getDelaysGifImages gifData
            putStrLn $ "Adding " ++ filename ++ " (" ++ show (length gifImages) ++ " frames)"

            let frames = zip (map (dynImageToPackagedData builder) gifImages)
                             (map ((/100) . fromIntegral) gifDelays)

            if length frames > 8 then do
                putStrLn "Reducing to 8 frames."
                return $ map (bimap head sum . unzip) $ chunksOf ((length frames + 7)`div` 8) frames
            else return frames

        (filename, n) -> do
            putStrLn $ "Adding " ++ filename
            dynImage <- either error id <$> readImage filename
            let bits = dynImageToPackagedData builder dynImage
            pure [(bits, n)]

replace :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
replace at replacement source
    | BS.length replacement > BS.length after
    = error "replaceIn: Replacement larger than remaining string"
    | otherwise = before <> replacement <> BS.drop (BS.length replacement) after
    where
        (before, after) = BS.splitAt at source

template :: BS.ByteString
template = $(embedFile (FIRMWARE ++ "/firmware.bin"))

mapFile :: String
mapFile = BSC.unpack $(embedFile (FIRMWARE ++ "/firmware.map"))

offsetInitialStep, offsetTiming, offsetImages :: Int
(offsetInitialStep, offsetTiming, offsetImages) =
    let find name
            | [_,s] <- getAllTextSubmatches (mapFile =~ ("^C: +([0-9ABCDEF]{8}) +_" ++ name)) :: [String]
            , [(i,"")] <- readHex s
            = i
            | otherwise
            = error $ "Cound not find location of " ++ show name ++ " in firmware.map"
    in (find "initial_step", find "timing", find "images")


work :: TransSpec -> Either () [(FilePath, Double)] -> FilePath -> IO ()
work spec (Left ()) output = do
    let builder = bitBuilder spec
    let pixels = [ z | (R,z) <- builder $ \c _arm z -> (c,z) ]

    let size = 1000
        black = PixelRGBA8 0 0 0 255
        white = PixelRGBA8 255 255 255 255
        img =
            renderDrawing size size black $
            withTexture (uniformTexture white) $
            for_ pixels $ \(x :+ y) ->
                let f x = realToFrac $ (x+1) * fromIntegral size/2 in
                fill $ circle (V2 (f x) (f y)) 3

    writePng output img

work spec (Right timed_sources) output = do
    let builder = bitBuilder spec
    timed_data <- concat <$> mapM (getImage builder) timed_sources
    let (imagesData, timings) = unzip timed_data
    let imageData = BS.concat imagesData

    when (length imagesData > 8) $ do
        putStrLn "Too many images"
        exitFailure

    let timingData  = doubleToUInt16Array $
            map (*256) $
            take 8 $ timings ++ repeat 0

    putStrLn $ "Writing " ++ output
    BS.writeFile output $
        replace offsetImages imageData $
        replace offsetTiming timingData
        template

doubleToUInt16Array =
    BS.pack . map fromIntegral . concatMap ((\x -> [x`mod`256, x`div`256]) . round)


-- Argument handling

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "YQ3008 image generator"
  -- <> progDesc "TODO"
  )
  where
    linearSpecParser :: Parser TransSpec
    linearSpecParser =
        flag' Linear
            (  long "linear"
            <> help "Scan the input image linearly, not circularly"
            )

    circularSpecParser :: Parser TransSpec
    circularSpecParser = Circular
        <$> option auto
            (  long "offset"
            <> metavar "OFFSET"
            <> help "vertical offset of the bar from the hub (in leds)"
            <> value 0
            )
        <*> option auto
            (  long "shift"
            <> metavar "SHIFT"
            <> help "horizontal shift of the bar from the hub (in leds)"
            <> value 0
            )
        <*> option auto
            (  long "rotation"
            <> metavar "ROTATION"
            <> help "position of the magnet [0..12]"
            <> value 12
            <> showDefault
            )

    maskParser :: Parser ()
    maskParser =
        flag' ()
            (  long "gen-mask"
            <> help "Create an outline of available pixesl"
            )

    firmwareParser :: Parser [(FilePath, Double)]
    firmwareParser = some (
        (,) <$> strArgument
                (  metavar "IMAGE"
                <> help "input file (can be CALIBRATION)"
                )
            <*> argument auto
                (  metavar "DURATION"
                <> help "duration (in seconds)"
                )
        )

    parser :: Parser (IO ())
    parser = work
        <$> (linearSpecParser <|> circularSpecParser)
        <*> ((Left <$> maskParser) <|> (Right <$> firmwareParser))
        <*> strOption
            (  long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "output file"
            )

