{-|
Module      : Graphics.Figgy.Legend
Description : Code for drawing and defining a legend for use in an EPS file.
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module Graphics.Figgy.Legend where

import Control.Monad.Reader
import TextShow

import Graphics.Figgy.Colors
import Graphics.Figgy.PostScript

-- | List of labels and corresponding colors
type LegendDesc = [(String, RGB1)]

-- | Position in the viewframe
data LegendPos = TopLeft | BottomLeft | TopRight | BottomRight
                 deriving ( Eq, Show )

-- | Various dimensions for legend display
data LegendDims = LC { figPadding :: (Double, Double),
                       legendPos :: LegendPos,
                       legendPadding :: (Double, Double, Double, Double),
                       itemSpacing :: Double,
                       labelDist :: Double,
                       boxSize :: (Double, Double),
                       textSize :: Double
                     } deriving ( Eq, Show )

-- | Default dimensions and position
defaultLegend :: LegendDims
defaultLegend = LC { figPadding = (10, 10),
                     legendPos = TopLeft,
                     legendPadding = (2,7,7,7), -- top,right,bottom,left
                     itemSpacing = 3,
                     labelDist = 6,
                     boxSize = (20, 8),
                     textSize = 13 }

drawLegend :: LegendDims -> LegendDesc -> EPSM
drawLegend c d = inLocalEnv $ do
  commL ["/Helvetica findfont",
         showt $ textSize c, "scalefont setfont"]
  -- EPSM is a ReaderT BBox
  frame <- ask
  -- Calculate some relevant information
  let items = fromIntegral $ length d -- number of labels/colors
      (inPadY1, inPadX2, inPadY2, inPadX1) = legendPadding c
      xmin = xMin frame
      xmax = xMax frame
      ymin = yMin frame
      ymax = yMax frame
      boxWidth = fst $ boxSize c
      boxHeight = snd $ boxSize c
      iS = itemSpacing c
      legendExtraWidth = inPadX1 + inPadX2 + boxWidth + labelDist c
      legendHeight = inPadY1 + inPadY2 - iS + items * (textSize c + iS)
      outPadX = fst $ figPadding c
      outPadY = snd $ figPadding c
  getMaxStringWidth $ map fst d -- leaves widest string width on PS stack
  -- Some functions that can be reused in different positions
  let addExtraLegendWidth = commL [showt legendExtraWidth, "add"]
      completeLegendBox = commL [showt (- legendHeight), "0 0 box"]
      mkLeftBox = addExtraLegendWidth >> completeLegendBox
      mkRightBox = do addExtraLegendWidth
                      comm "dup neg 0 translate"
                      completeLegendBox
  -- Make legend box
  case legendPos c of
    TopLeft ->
      do translate (xmin + outPadX) (ymax - outPadY)
         mkLeftBox
    TopRight ->
      do translate (xmax - outPadX) (ymax - outPadY)
         mkRightBox
    BottomLeft ->
      do translate (xmin + outPadX) (ymin + outPadY + legendHeight)
         mkLeftBox
    BottomRight ->
      do translate (xmax - outPadX) (ymin + outPadY + legendHeight)
         mkRightBox
  -- Draw an outline and color the box
  strokeAndFill 0.5 white
  -- Draw the colors and labels
  let go pos item = do box boxWidth boxHeight (inPadX1, -pos)
                       strokeAndFill 0.5 $ snd item
                       let labelX = inPadX1 + boxWidth + labelDist c
                       showText (fst item) (labelX, -pos)
                       return $ pos + textSize c + iS
  foldM_ go (inPadY1 + textSize c) d

-- | Use default dimensions
drawDefaultLegend :: LegendDesc -> EPSM
drawDefaultLegend = drawLegend defaultLegend
