{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-

Space in the middle: 9
Off-center: 7

-}

import Codec.Picture
import Codec.Picture.Gif
import Data.List.Split
import Data.Complex
import System.Environment
import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.Bits.Bitwise as BA
import Options.Applicative
import Control.Monad
import Data.Monoid
import System.Exit
import Data.Char
import Data.FileEmbed

cFRAMES = 256
cCENTER = 9
cLEN=33+cCENTER/2

data Color = R|G|B deriving (Show, Eq)


type Offset = Double
type Shift = Double
type Rotation = Double

type BitMaker = (Color -> Bool -> Complex Double -> Bool) -> [Bool]


bitBuilder :: Offset -> Shift -> Rotation -> BitMaker
bitBuilder off shift rot f =
    [ f c a (((cpos + displacement) * cis rho * cis rot_radians) / (radius :+ 0))
    | frame <- [0,1..cFRAMES-1]
    , let rho = fromIntegral frame*2*pi/ fromIntegral cFRAMES
    , c <- [R,G,B]
    , (a, pos) <- ((False,) <$> arm) ++ (((True,) . negate) <$> arm)
    , let cpos = pos :+ 0
    ]
  where
    displacement = (shift :+ off)
    rot_radians = (-rot-3)/12*2*pi
    arm = concatMap reverse $ chunksOf 8 $ map (cCENTER/2 +) [0..31]
    radius = sqrt ((cLEN + abs shift)^2 + off^2)

getColor :: Color -> PixelRGB8 -> Bool
getColor R (PixelRGB8 r _ _ ) = r > 130
getColor G (PixelRGB8 _ g _ ) = g > 130
getColor B (PixelRGB8 _ _ b ) = b > 130

calibrationBits :: [Bool]
calibrationBits = map not $ foldr1 (zipWith (||)) $
    [ bitBuilder off 0 0 $ \c a z ->
        abs (phase z - off*pi/10) < pi/40 &&
       (c == oc) &&
       (if a then magnitude z > 0.75
             else (magnitude z < 0.75 && (magnitude z > 0.5 || off == 0)))
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
            putStrLn $ "Adding offset calibration image"
            pure [(bitsToData calibrationBits, n)]
        (filename, _) | takeExtension filename == ".gif" -> do
            gifData <- BS.readFile filename
            let gifImages = either error id $ decodeGifImages gifData
            let gifDelays = either error id $ getDelaysGifImages gifData
            putStrLn $ "Adding " ++ filename ++ " (" ++ show (length gifImages) ++ " frames)"

            pure $ zip (map (dynImageToPackagedData builder) gifImages)
                       (map ((/100) . fromIntegral) gifDelays)
        (filename, n) -> do
            putStrLn $ "Adding " ++ filename
            dynImage <- either error id <$> readImage filename
            let bits = dynImageToPackagedData builder dynImage
            pure [(bits, n)]

replace :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
replace pat replacement source
    | BS.null after
    = error $ "replaceIn: Pattern not found: " ++ show pat
    | BS.length replacement > BS.length after
    = error "replaceIn: Replacement larger than remaining string"
    | otherwise = before <> replacement <> BS.drop (BS.length replacement) after
    where
        (before, after) = BS.breakSubstring pat source

template :: BS.ByteString
template = $(embedFile "firmware/firmware.bin")

work :: FilePath -> Offset -> Shift -> Rotation -> [(FilePath, Double)] -> IO ()
work output offset shift rotation timed_sources = do
    let builder = bitBuilder offset shift rotation
    timed_data <- concat <$> mapM (getImage builder) timed_sources
    let (imagesData, timings) = unzip timed_data
    let imageData = BS.concat imagesData

    when (length imagesData > 8) $ do
        putStrLn $ "Too many images"
        exitFailure


    let imagePattern = BS.pack (map (fromIntegral.ord) "THIS IS WHERE THE IMAGE STARTS")
    let timingData    = BS.pack $ map fromIntegral $
            concatMap (\x -> [x`mod`256, x`div`256]) $
            map round $
            map (*256) $
            take 8 $ timings ++ repeat 0
    let timingPattern = BS.pack (take 16 (cycle [42,23]))

    putStrLn $ "Writing " ++ output
    BS.writeFile output $
        replace imagePattern imageData $
        replace timingPattern timingData $
        template

-- Argument handling

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "YQ3008 image generator"
  -- <> progDesc "Fills a file with as much zeroes as possible"
  )
  where
    parser :: Parser (IO ())
    parser = work
        <$> strOption
            (  long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "output file"
            )
        <*> option auto
            (  long "offset"
            <> metavar "OFFSET"
            <> help "vertical offset of the bar from the hub (in leds)"
            )
        <*> option auto
            (  long "shift"
            <> metavar "SHIFT"
            <> help "horizontal shift of the bar from the hub (in leds)"
            )
        <*> option auto
            (  long "rotation"
            <> metavar "ROTATION"
            <> help "position of the magnet [0..12]"
            )
        <*> some (
            (,) <$> strArgument
                    (  metavar "IMAGE"
                    <> help "input file (can be CALIBRATION)"
                    )
                <*> argument auto
                    (  metavar "DURATION"
                    <> help "duration (in seconds)"
                    )
            )

