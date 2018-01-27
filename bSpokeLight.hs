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
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Regex.Posix
import Numeric (readHex)

cFRAMES = 256          :: Integer
cCENTER = 9            :: Double
cLEN    = 33+cCENTER/2 :: Double
cT0RATE = 1000000      :: Integer -- TODO: Need to calibrate this

data Color = R|G|B deriving (Show, Eq)


type Offset = Double
type Shift = Double
type Rotation = Double
type Speed = Double

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

replace :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
replace at replacement source
    | BS.length replacement > BS.length after
    = error "replaceIn: Replacement larger than remaining string"
    | otherwise = before <> replacement <> BS.drop (BS.length replacement) after
    where
        (before, after) = BS.splitAt at source

template :: BS.ByteString
template = $(embedFile "firmware/firmware.bin")

offsetInitialStep, offsetTiming, offsetImages :: Int
(offsetInitialStep, offsetTiming, offsetImages) = $(do
    qAddDependentFile "firmware/firmware.map"
    mapFile <- runIO $ readFile "firmware/firmware.map"
    let find name
            | [_,s] <- getAllTextSubmatches (mapFile =~ ("^C: +([0-9ABCDEF]{8}) +_" ++ name)) :: [String]
            , [(i,"")] <- readHex s
            = return i
            | otherwise
            = do reportError $ "Cound not find location of " ++ show name ++ " in firmware.map"
                 fail ""
    ois <- find "initial_step"
    ot  <- find "timing"
    oi  <- find "images"
    return $ TupE $ map LitE [integerL ois, integerL ot, integerL oi]
    )

work :: FilePath -> Offset -> Shift -> Rotation -> Speed -> [(FilePath, Double)] -> IO ()
work output offset shift rotation speed timed_sources = do
    let builder = bitBuilder offset shift rotation
    timed_data <- concat <$> mapM (getImage builder) timed_sources
    let (imagesData, timings) = unzip timed_data
    let imageData = BS.concat imagesData

    when (length imagesData > 8) $ do
        putStrLn $ "Too many images"
        exitFailure

    let timingData    = BS.pack $ map fromIntegral $
            concatMap (\x -> [x`mod`256, x`div`256]) $
            map round $
            map (*256) $
            take 8 $ timings ++ repeat 0

    let initial_step_data = BS.pack $ map fromIntegral $
            concatMap (\x -> [x`mod`256, x`div`256]) $
            map round $
            [ speed / fromIntegral cFRAMES * fromIntegral cT0RATE ]

    putStrLn $ "Writing " ++ output
    BS.writeFile output $
        replace offsetImages imageData $
        replace offsetTiming timingData $
        replace offsetInitialStep initial_step_data $
        template

-- Argument handling

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "YQ3008 image generator"
  -- <> progDesc "TODO"
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
        <*> option auto
            (  long "speed"
            <> metavar "SECONDS"
            <> help "how long one image scan is initially"
            <> value 0.5
            <> showDefault
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

