{-|
Module      : Graphics.Figgy.Generic
Description : Some generic functions for reuse within the library
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

module Graphics.Figgy.Generic where

-- | Floating point ratio between two integral values
fRatio :: (Integral a, Floating b) => a -> a -> b
fRatio x y = fromIntegral x / fromIntegral y

-- | If `length list >= n` split it into `n` approximately equal sublists.
--   If `n > length list` only return non-empty sublists.
chunks :: Int -> [a] -> [[a]]
n `chunks` list = filter (not . null) $ go 0 list
  where step = fRatio (length list) n :: Double
        go nDone xs = let loc = fromIntegral nDone * step
                          locNext = loc + step
                          chunkSize = floor locNext - floor loc
                          (chunk, rest) = splitAt chunkSize xs
                       in if nDone == n - 1
                             then [xs]
                             else chunk : go (nDone + 1) rest

-- | A list with the same length as `xs` with uniformly spaced Double
--   values from 0 to 1
uni1 :: [a] -> [Double]
uni1 xs = [fRatio x (length xs - 1) | x <- [0..length xs - 1]]
