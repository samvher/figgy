{-|
Module      : Graphics.Figgy.ColorMap
Description : Code for mapping 0-1 values to RGB. Most MATLAB colormaps are
              implemented, along with some others.
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

module Graphics.Figgy.ColorMap ( mkColorMap,
                                 cmapSpec,
                                 cmapSpec255,
                                 ColorMap,
                                 i2iCM,
                                 i3iCM,
                                 iceCM,
                                 parulaCM,
                                 jetCM,
                                 hsvCM,
                                 hotCM,
                                 coolCM,
                                 springCM,
                                 summerCM,
                                 autumnCM,
                                 winterCM,
                                 grayCM,
                                 boneCM,
                                 copperCM,
                                 pinkCM
                               ) where

import Data.Bifunctor ( first, second )
import Data.List ( sortBy )
import Data.Function ( on )
import Safe ( headMay, lastMay )

import Graphics.Figgy.Colors

-- | The code assumes Double `elem` [0, 1]
type ColorMap = Double -> RGB1

-- | A list of set points from which an interpolated colormap is generated
newtype CMapSpec = CMapSpec [(Double, RGB1)]
                   deriving (Eq, Show)

-- | Input values for CMapSpec's must be `elem` [0,1] and sorted
cmapSpec :: [(Double, RGB1)] -> CMapSpec
cmapSpec = CMapSpec . checkCs . sortBy (compare `on` fst)
  where valid cs = all (\(v, _) -> v >= 0 && v <= 1) cs && not (null cs)
        checkCs cs =
          if valid cs then cs
          else error $ "ColorMap index values should be between 0 and 1"

-- | Allow definition using RGB values based on 0-255
cmapSpec255 :: [(Double, RGB)] -> CMapSpec
cmapSpec255 = cmapSpec . map toRGB1
  where toRGB1 = second (\(r, g, b) -> (fromIntegral r / 255,
                                        fromIntegral g / 255,
                                        fromIntegral b / 255))

-- | Generate a ColorMap mapping from CMapSpec specifications
mkColorMap :: CMapSpec -> ColorMap
mkColorMap (CMapSpec spec) = getColor
  where -- Find the enclosing set points in the cmap specification
        between d = first lastMay . second headMay $ span ((<d) . fst) spec
        -- Interpolate if between two set points, use the outer set point
        -- if outside the specified range. (Error if the value is outside
        -- [0,1]).
        getColor d
          | d < 0 = error "Negative colormap input value"
          | d > 1 = error "Colormap input value > 1"
          | otherwise =
              case between d of
                (Nothing, Just (_, rgb)) -> rgb
                (Just (_, rgb), Nothing) -> rgb
                (Just (v1, (r1, g1, b1)),
                 Just (v2, (r2, g2, b2))) -> let x = (v2 - d) / (v2 - v1)
                                                 y = 1 - x
                                              in (x * r1 + y * r2,
                                                  x * g1 + y * g2,
                                                  x * b1 + y * b2)
                (Nothing, Nothing) -> error "Received empty CMapSpec"

-- | i2i colors
i2iCMSpec :: CMapSpec
i2iCMSpec = cmapSpec255 [(0,   (217, 217, 217)),
                         (0.5, (135, 167, 187)),
                         (1,   (  0,  62,  92))]

i2iCM :: ColorMap
i2iCM = mkColorMap i2iCMSpec

-- | More i2i colors
i3iCMSpec :: CMapSpec
i3iCMSpec = cmapSpec255 [(0,   (   0,  89, 132)),
                         (0.5, ( 202, 216, 224)),
                         (1,   ( 255, 118,  25))]

i3iCM :: ColorMap
i3iCM = mkColorMap i3iCMSpec

-- | A grey-to-blue colormap
iceCMSpec :: CMapSpec
iceCMSpec = cmapSpec [(0.0, (0.9, 0.9, 0.9)),
                      (1.0, (0.1, 0.3, 0.9))]

iceCM :: ColorMap
iceCM = mkColorMap iceCMSpec

-- * MATLAB colormaps

parulaCMSpec, jetCMSpec, hsvCMSpec, hotCMSpec, coolCMSpec :: CMapSpec

parulaCMSpec = cmapSpec255 [(0/16,  (  53,  42, 135)),
                            (6/16,  (   7, 156, 207)),
                            (11/16, ( 165, 190, 107)),
                            (16/16, ( 249, 251,  14))]

jetCMSpec = cmapSpec255 [(0/16,  (  0,   0, 191)),
                         (1/16,  (  0,   0, 255)),
                         (6/16,  (  0, 255, 255)),
                         (10/16, (255, 255,   0)),
                         (14/16, (255,   0,   0)),
                         (16/16, (128,   0,   0))]

hsvCMSpec = cmapSpec255 [(0,   ( 255,   0,   0)),
                         (1/3, (   0, 255,   0)),
                         (2/3, (   0,   0, 255)),
                         (1,   ( 255,   0,   0))]

hotCMSpec = cmapSpec255 [(0,   (   0,   0,   0)),
                         (1/3, ( 255,   0,   0)),
                         (2/3, ( 255, 255,   0)),
                         (1,   ( 255, 255, 255))]

coolCMSpec = cmapSpec255 [(0,  (   0, 255, 255)),
                          (1,  ( 255,   0, 255))]

parulaCM, jetCM, hsvCM, hotCM, coolCM :: ColorMap
parulaCM = mkColorMap parulaCMSpec
jetCM = mkColorMap jetCMSpec
hsvCM = mkColorMap hsvCMSpec
hotCM = mkColorMap hotCMSpec
coolCM = mkColorMap coolCMSpec

springCMSpec, summerCMSpec, autumnCMSpec, winterCMSpec :: CMapSpec

springCMSpec = cmapSpec255 [(0, ( 255,   0, 255)),
                            (1, ( 255, 255,   0))]

summerCMSpec = cmapSpec255 [(0, (   0, 128, 102)),
                            (1, ( 255, 255,   0))]

autumnCMSpec = cmapSpec255 [(0, ( 255,   0,   0)),
                            (1, ( 255, 255,   0))]

winterCMSpec = cmapSpec255 [(0, (   0,   0, 255)),
                            (1, (   0, 255, 128))]

springCM, summerCM, autumnCM, winterCM :: ColorMap
springCM = mkColorMap springCMSpec
summerCM = mkColorMap summerCMSpec
autumnCM = mkColorMap autumnCMSpec
winterCM = mkColorMap winterCMSpec

grayCMSpec, boneCMSpec, copperCMSpec, pinkCMSpec :: CMapSpec

grayCMSpec = cmapSpec255 [(0, (   0,   0,   0)),
                          (1, ( 255, 255, 255))]

boneCMSpec = cmapSpec255 [(0,   (   0,   0,   5)),
                          (1/3, (  60,  60,  86)),
                          (2/3, ( 149, 175, 181)),
                          (1,   ( 255, 255, 255))]

copperCMSpec = cmapSpec255 [(0, (   0,   0,   0)),
                            (1, ( 255, 199, 127))]

pinkCMSpec = cmapSpec255 [(0,   (  60,   0,   0)),
                          (1/3, ( 190, 120, 120)),
                          (2/3, ( 231, 231, 178)),
                          (1,   ( 255, 255, 255))]

grayCM, boneCM, copperCM, pinkCM :: ColorMap
grayCM = mkColorMap grayCMSpec
boneCM = mkColorMap boneCMSpec
copperCM = mkColorMap copperCMSpec
pinkCM = mkColorMap pinkCMSpec
