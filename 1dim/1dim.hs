import qualified Data.Vector  as V
import Data.Vector (Vector, (//))
import Control.Concurrent
import Data.Word

type S = (Int, Int, Int)

draw :: S -> String
draw (a,b,c) =
    (\x -> "\r[" ++ x ++ "]") $
    V.toList $ V.replicate 64 ' ' // [(f a, '*'), (f b, '#'), (f c, '%')]
  where f n = max 0 $ min 63 $ fromIntegral (n `div` 4)

initS = (20,50,200)

step (a,b,c) = (f a da, f b db,f c dc)
  where
    a' | a > 128 = a`div`3
       | otherwise = (2*256+a)`div`3
    da = 2 * if 2 * 2 * abs (b - a) < abs (c - a) then signum (b - a) else signum (c - a)
    db | a > 128, b > 128+64 = -10 -- (128-abs(a-b)) `div` 5
       | a < 128, b < 64 =  10 -- (128-abs(a-b)) `div` 5
       | abs (a - b) > 128 = signum (a' - b)
       | a == b = 5
       | otherwise = - 3 * signum (a - b)
    dc | odd c = -4
       | even c = 4

    f x dx = x + max (-x) (min (265-x) dx)


main = go initS

go s = do
  putStr $ draw s
  threadDelay 30000
  go (step s)

