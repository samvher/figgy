{-|
Module      : Graphics.Figgy.Colors
Description : Some shared code for dealing with RGB values.
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

module Graphics.Figgy.Colors where

-- | Rs Gs and Bs `elem` [0..255]
type RGB = (Int, Int, Int)

-- | Rs Gs and Bs `elem` [0,1]
type RGB1 = (Double, Double, Double)

-- | Convert RGB255 to RGB1
mkRGB1 :: RGB -> RGB1
mkRGB1 (r, g, b) = (fromIntegral r / 255,
                    fromIntegral g / 255,
                    fromIntegral b / 255)

-- | Define some colors
black, white :: RGB1
black = (0.0, 0.0, 0.0)
white = (1.0, 1.0, 1.0)

-- | Average two colors
mixRGB1 :: RGB1 -> RGB1 -> RGB1
mixRGB1 (r1, g1, b1) (r2, g2, b2) = ((r1 + r2)/2, (g1 + g2)/2, (b1 + b2)/2)
