{-|
Module      : Graphics.Figgy.ValueScaling
Description : Code for binning, scaling, coloring values and generating
              corresponding legend descriptions.
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

module Graphics.Figgy.ValueScaling where

import Data.Bifunctor ( second )
import Data.Function ( on )
import Data.List ( partition, sort, sortBy )
import Text.Printf

import Graphics.Figgy.ColorMap
import Graphics.Figgy.Colors
import Graphics.Figgy.Generic
import Graphics.Figgy.Legend

type Range = (Double, Double)
type Boundary = Double

-- | Chop up a list of labeled numbers based on a list of boundary values.
--   Assumes both inputs are sorted.
chop :: [Boundary] -> [(a, Double)] -> [[(a, Double)]]
chop [] vs = [vs]
chop _ [] = []
chop (b:bs) vs = now : chop bs later
  where (now, later) = span ((<b) . snd) vs

-- | Same as chop, but now if there are data values smaller than the first
--   boundary, they are included in the first segment, and similarly for
--   values larger than the last boundary.
chop' :: [Boundary] -> [(a, Double)] -> [[(a, Double)]]
chop' bs' vs' = go (tail bs') vs'
  where go [] vs = [vs]
        go [_] vs = [vs]
        go _ [] = []
        go (b:bs) vs = now : go bs later
          where (now, later) = span ((<b) . snd) vs

edges2ranges :: [Boundary] -> [Range]
edges2ranges (b1:b2:bs) = (b1, b2) : edges2ranges (b2:bs)
edges2ranges [_] = []
edges2ranges [] = []

-- | Take as input a list of labeled values, and generate bin boundaries
type BinningStrategy = [(String, Double)] -> [Boundary]

-- | Assumes sorted is. A binning strategy that tries to uniformly
--   distribute values over the bins (so that each bin has an approximately
--   equal number of values assigned to it).
binEdgesByAmount :: Int -> BinningStrategy
binEdgesByAmount n is = allLowers ++ [upper]
  where chunked = n `chunks` is
        upper = snd . last $ last chunked
        allLowers = map (snd . head) chunked

-- | Assumes sorted is. A binning strategy that distributes bin edges
--   uniformly over the range of input values.
binEdgesUniform :: Int -> BinningStrategy
binEdgesUniform n is = map ((+mini) . (*range)) $ uni1 [0..n]
  where values = map snd is
        mini = head values
        maxi = last values
        range = maxi - mini

-- | Takes average bin boundaries from ByAmount and Uniform strategies
binEdgesMixed :: Int -> BinningStrategy
binEdgesMixed n = \is -> zipWith avg (binEdgesByAmount n is)
                                     (binEdgesUniform n is)
  where avg a b = (a + b) / 2

-- | Similar to Uniform, but now the upper and lower bounds are chosen.
binEdgesCustom :: Int -> Double -> Double -> BinningStrategy
binEdgesCustom n lower upper = const . map scale $ uni1 [0..n]
  where scale x = x * (upper - lower) + lower

-- | First splits the input data into positive and negative values. These
--   two subsets are then binned with a Uniform strategy.
binSymm0 :: Int -> BinningStrategy
binSymm0 n = \is -> let (sub, super) = partition ((< 0) . snd) is
                     in init (binEdgesUniform n sub) ++ [0] ++
                          tail (binEdgesUniform n super)

-- | Get the range of a list of labeled input values
dataRange :: [(String, Double)] -> Range
dataRange is = (head values, last values)
  where values = sort $ map snd is

-- | Transform a colormap so that the center of the colormap corresponds to
--   0 when showing the input range.
centerCM :: Range -> ColorMap -> ColorMap
centerCM (low, high) cm =
    if low > 0 || high < 0
    then cm
    else \x -> let x' = if x <= centerpoint
                        then x / centerpoint * 0.5
                        else (x - centerpoint) /
                               (1 - centerpoint) * 0.5 + 0.5
               in cm x'
  where centerpoint = (0 - low) / (high - low)

-- | Generate a legend description from a list of ranges and a colormap
mkLegendDesc :: [Range] -> ColorMap -> LegendDesc
mkLegendDesc rs cm = zip (map mkTag rs) (map cm vs)
  where pr = printf "%.1f"
        mkTag (start, end) = pr start ++ " tot " ++ pr end
        vs = uni1 rs

-- | Transform binned values so that their new values are evenly (with
--   respect to the bins) distributed between 0 and 1.
range01 :: [[(a, Double)]] -> [(a, Double)]
range01 iss = concat $ zipWith go iss (uni1 iss)
  where go is v = map (second $ const v) is

-- | From a binning strategy, a colormap, and labeled input values generate
--   everything you need to display them: a legend description and labels
--   with corresponding colors.
getColors :: BinningStrategy -> ColorMap -> [(String, Double)]
          -> (LegendDesc, [(String, RGB1)])
getColors bs cm is' = (mkLegendDesc ranges cm, colored)
  where is = sortBy (compare `on` snd) is'
        edges = bs is
        ranges = edges2ranges edges
        colored = map (second cm) . range01 $ chop' edges is

-- | With number of bins, colormap and labeled values generate everything
--   you need for display using the ByAmount strategy.
autoEqual :: Int -> ColorMap -> [(String, Double)]
          -> (LegendDesc, [(String, RGB1)])
autoEqual bins = getColors (binEdgesByAmount bins)

-- | With number of bins, colormap and labeled values generate everything
--   you need for display using the Uniform strategy.
autoUniform :: Int -> ColorMap -> [(String, Double)]
            -> (LegendDesc, [(String, RGB1)])
autoUniform bins = getColors (binSymm0 bins)

-- | With number of bins, colormap and labeled values generate everything
--   you need for display using the EdgesMixed/Intelligent strategy.
autoIntelligent :: Int -> ColorMap -> [(String, Double)]
                -> (LegendDesc, [(String, RGB1)])
autoIntelligent bins = getColors (binEdgesMixed bins)

-- | With number of bins, colormap and labeled values generate everything
--   you need with Symm0 strategy.
autoResp0 :: Int -> ColorMap -> [(String, Double)]
                 -> (LegendDesc, [(String, RGB1)])
autoResp0 bins = getColors (binEdgesMixed bins)

-- | With bounds, bins, colormap and labeled values generate everything
--   you need for display using the Custom strategy
customScale :: Double -> Double -> Int -> ColorMap -> [(String, Double)]
            -> (LegendDesc, [(String, RGB1)])
customScale lower upper bins = getColors (binEdgesCustom bins lower upper)
