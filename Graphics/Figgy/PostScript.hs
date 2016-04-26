{-|
Module      : Graphics.Figgy.PostScript
Description : PostScript code generation
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Figgy.PostScript where

import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.DList as DL
import qualified Data.Text as T
import TextShow

import Graphics.Figgy.Colors

type Point = (Double, Double)
type Polygon = [[(Double, Double)]]
type Label = (Point, String) -- For a point with some text next to it

-- | Bounding box
data BBox =
  BBox {
    xMin :: Double,
    yMin :: Double,
    xMax :: Double,
    yMax :: Double
  } deriving (Eq, Show)

width :: BBox -> Double
width bb = xMax bb - xMin bb

height :: BBox -> Double
height bb = yMax bb - yMin bb

-- | Horizontal midpoint
xMid :: BBox -> Double
xMid bb = (xMin bb + xMax bb) / 2

-- | Vertical midpoint
yMid :: BBox -> Double
yMid bb = (yMin bb + yMax bb) / 2

-- | Aspect ratio
aspectRatio :: BBox -> Double
aspectRatio bb = height bb / width bb

-- | Efficient intersection test
intersect :: BBox -> BBox -> Bool
intersect bb1 bb2 =
  abs (xMid bb1 - xMid bb2) * 2 < (width bb1 + width bb2) &&
    abs (yMid bb1 - yMid bb2) * 2 < (height bb1 + height bb2)

-- | `mappend`ing two BBox'es results in the smallest BBox enclosing both
instance Monoid BBox where
  mempty = BBox { xMin = read "Infinity",
             yMin = read "Infinity",
             xMax = - (read "Infinity"),
             yMax = - (read "Infinity") }
  bb1 `mappend` bb2 = BBox { xMin = min (xMin bb1) (xMin bb2),
                     yMin = min (yMin bb1) (yMin bb2),
                     xMax = max (xMax bb1) (xMax bb2),
                     yMax = max (yMax bb1) (yMax bb2) }

-- | Scale all dimensions
scaleBB :: Double -> BBox -> BBox
scaleBB s (BBox xmin ymin xmax ymax) = BBox (xmin * s) (ymin * s)
                                            (xmax * s) (ymax * s)

-- | Scale a list of points
scalePoints :: Double -> [(Double, Double)] -> [(Double, Double)]
scalePoints s ps = map scaleP ps
  where scaleP (x, y) = (s*x, s*y)

-- | Scale all points in a polygon
scalePoly :: Double -> Polygon -> Polygon
scalePoly s p = map (scalePoints s) p

-- | Capture code generation in a Writer monad (using Data.Text)
class MonadWriter (DL.DList T.Text) m => MonadPS m where

  -- | Issue a postscript command
  comm :: T.Text -> m ()
  comm = tell . DL.singleton

  -- | Build a command from a list of command fragments
  commL :: [T.Text] -> m ()
  commL = comm . T.intercalate " "

-- | Reader/Writer stack. EPS contains BBox information, this is available
--   inside the monad. Writer keeps track of all issued commands.
newtype EPSM' a =
  EPSM' {
      unEPSM' :: ReaderT BBox (Writer (DL.DList T.Text)) a
    } deriving ( Applicative,
                 Functor,
                 Monad,
                 MonadReader BBox,
                 MonadWriter (DL.DList T.Text) )

-- | Prettier type declarations
type EPSM = EPSM' ()

-- | Use `comm` and `commL`
instance MonadPS EPSM'

-- | PostScript, unlike EPS, does not enclose BBox information.
newtype PSM' a =
  PSM' {
    unPSM' :: Writer (DL.DList T.Text) a
    } deriving ( Applicative,
                 Functor,
                 Monad,
                 MonadWriter (DL.DList T.Text) )

-- | Prettier type declarations
type PSM = PSM' ()

-- | Use `comm` and `commL`
instance MonadPS PSM'

-- | Generate EPS code for an image with the given BBox
runEps :: BBox -> EPSM -> T.Text
runEps bb m = T.intercalate "\n" . DL.toList . runStack $
    header >> defs >> m >> footer
  where runStack = execWriter . flip runReaderT bb . unEPSM'

-- | EPS header
header :: EPSM
header = do
  (BBox xmin ymin xmax ymax) <- ask
  comm "%!PS-Adobe-3.0 EPSF-3.0"
  commL ["%%BoundingBox:", showt xmin, showt ymin, showt xmax, showt ymax]
  comm "%%BeginProlog"
  comm "%%EndProlog"
  comm "/Helvetica findfont 12 scalefont setfont"

-- | Some functions used by figgy
defs :: MonadPS m => m ()
defs = do -- Stroke and fill current shape
          -- Arguments: linewidth r g b
          commL ["/strokeandfill {",
                   "gsave",
                   "setrgbcolor fill",
                   "grestore",
                   "setlinewidth stroke",
                 "} def"]
          -- Reduce output filesize
          comm "/m {moveto} def"
          comm "/l {lineto} def"
          -- Scale top two numbers on the stack by 0.5
          commL ["/halvecoords {",
                  "2 div exch 2 div exch",
                 "} def"]
          -- Multiply top two numbers on the stack by -1
          commL ["/negcoords {",
                   "neg exch neg exch",
                 "} def"]
          -- Draw text centered at a given point
          -- Arguments: string x y
          commL ["/centertextat {",
                   "moveto",
                   "dup",
                   "stringwidth",
                   "halvecoords",
                   "negcoords",
                   "rmoveto",
                   "show",
                 "} def"]
          -- Draw text starting at a given point
          -- Arguments: string x y
          commL ["/showtextat {",
                   "moveto show newpath",
                 "} def"]
          -- Draw a box
          -- Arguments: width height x_bottomleft y_bottomleft
          commL ["/box {",
                   "moveto",
                   "dup 0 exch rlineto",
                   "1 index 0 rlineto",
                   "neg 0 exch rlineto",
                   "pop",
                   "closepath",
                 "} def"]
          -- Push the maximum of the top two numbers on the stack
          commL ["/max {",
                  "1 index 1 index",
                  "gt",
                  "{ pop }",
                  "{ exch pop }",
                  "ifelse",
                  "} def"]

-- | Issue a scale command
scale :: MonadPS m => Double -> Double -> m ()
scale x y = commL [showt x, showt y, "scale"]

-- | Issue a scale command where axes are scaled equally
scaleUniform :: MonadPS m => Double -> m ()
scaleUniform s = commL [showt s, showt s, "scale"]

-- | Translate to a Point
translate' :: MonadPS m => Point -> m ()
translate' (x, y) = commL [showt x, showt y, "translate"]

-- | Translate to x and y values
translate :: MonadPS m => Double -> Double -> m ()
translate = curry translate'

-- | Generate a polygon from a list of points
polyPath :: MonadPS m => [Point] -> m ()
polyPath (p:ps) = do commL [showt $ fst p, showt $ snd p, "m"]
                     mapM_ lineTo ps
                     comm "closepath"
polyPath _ = error "polyPath: input has one or fewer points"

-- | Issue lineto command
lineTo :: MonadPS m => Point -> m ()
lineTo (x, y) = commL [showt x, showt y, "l"]

-- | Stroke and fill current shape
strokeAndFill :: MonadPS m => Double -> RGB1 -> m ()
strokeAndFill w (r, g, b) =
  commL [showt w, showt r, showt g, showt b, "strokeandfill"]

-- | Fill current shape
fill :: MonadPS m => RGB1 -> m ()
fill (r, g, b) = commL [showt r, showt g, showt b, "setrgbcolor fill"]

-- | Set color
setcolor :: MonadPS m => RGB1 -> m ()
setcolor (r, g, b) = commL [showt r, showt g, showt b, "setrgbcolor"]

-- | Stroke current shape
stroke :: MonadPS m => Double -> m ()
stroke thickness = commL [showt thickness, "setlinewidth stroke"]

-- | Show polygon filled with given color
showPoly :: MonadPS m => RGB1 -> Polygon -> m ()
showPoly (r, g, b) ps = mapM_ polyPath ps >> fill (r, g, b)

-- | Show stroked polygon filled with given color, stroke thickness `w`
showPolyS :: MonadPS m => Double -> RGB1 -> Polygon -> m ()
showPolyS w (r, g, b) ps = mapM_ polyPath ps >> strokeAndFill w (r, g, b)

-- | Draw text centered at input point
centerText :: MonadPS m => String -> Point -> m ()
centerText s (x, y) = commL [T.pack s', showt x, showt y, "centertextat"]
  where s' = "(" ++ s ++ ")"

-- | Draw text starting from input point
showText :: MonadPS m => String -> Point -> m ()
showText s (x, y) = commL [T.pack s', showt x, showt y, "showtextat"]
  where s' = "(" ++ s ++ ")"

-- | Draw a point with a label next to it
showLabel :: MonadPS m => Label -> m ()
showLabel ((x, y), l) = do circle (x, y) 2
                           fill black
                           circle (x, y) 1
                           fill white
                           setcolor black
                           showText l (x + 5, y)

-- | Work in a protected environment (where we can safely scale/translate).
--   After the given commands are issued, the original environment is
--   restored.
inLocalEnv :: MonadPS m => m () -> m ()
inLocalEnv commands = do comm "gsave"
                         commands
                         comm "grestore"

-- | Map the corners of the input BBox to the corners of the viewframe and
--   draw in the BBox coordinate system.
withCoordChange :: BBox -> EPSM -> EPSM
withCoordChange space commands = inLocalEnv $ do
    frame <- ask
    let scaleX = (xMax frame - xMin frame) / (xMax space - xMin space)
        scaleY = (yMax frame - yMin frame) / (yMax space - yMin space)
    scale scaleX scaleY
    translate (- xMin space) (- yMin space)
    commands

-- | Draw with relative coordinates: (0,0) is bottom left, (1,1) top right.
--   Careful: if the frame is not square line thicknesses will be different
--            for vertical and horizontal lines.
withRelCoords :: EPSM -> EPSM
withRelCoords = withCoordChange bbRel
  where bbRel = BBox { xMin = 0, xMax = 1, yMin = 0, yMax = 1 }

-- | Change coordinate system so (0,0) is top left and the y-axis is
--   reversed.
fromTopLeft :: EPSM -> EPSM
fromTopLeft commands = inLocalEnv $ do
  frame <- ask
  translate 0 (yMax frame)
  scale 1.0 (-1.0)
  commands

-- | Another change of coordinates. The input BBox represents the space we
--   want to draw in, just as withCoordChange. Unlike that function, now
--   we don't map the corners to the viewframe, but resize it to fit in the
--   viewframe with equally scaled axes.
--     We get a situation like this:
--        ----------
--        |^|oooo|^|
--        |^|oooo|^|
--        ----------
--   where the ^ represent padding added to make sure that the o-region is
--   not stretched. The o-region is the one we draw in.
fitting :: BBox -> EPSM -> EPSM
fitting space commands = do
  frame <- ask
  withCoordChange (fitBB frame space) commands

-- | Fit space s in the frame f. If the aspect ratios are not equal, the
--   available drawing space will be increased. The returned BBox describes
--   the available drawing space.
fitBB :: BBox -> BBox -> BBox
fitBB f s = BBox { xMin = xMid s - newWidth / 2,
                   yMin = yMid s - newHeight / 2,
                   xMax = xMid s + newWidth / 2,
                   yMax = yMid s + newHeight / 2 }
  where -- relative dimensions
        relDimX = width f / width s
        relDimY = height f / height s
        -- effective new dimensions
        newWidth = if relDimX >= relDimY
                   then width s * aspectRatio s / aspectRatio f
                   else width s
        newHeight = if relDimY > relDimX
                    then height s * aspectRatio f / aspectRatio s
                    else height s

-- | Convert from coordinates in the drawing`space` to the view`frame`,
--   so the resulting coordinates are as if the point had been drawn in
--   `space` coordinates using `fitBB`.
toFigureCoords :: BBox -> BBox -> Point -> Point
toFigureCoords frame space (x, y) = (x', y')
  where space' = fitBB frame space
        x' = (x - xMin space') / (xMax space' - xMin space') *
               (xMax frame - xMin frame) + xMin frame
        y' = (y - yMin space') / (yMax space' - yMin space') *
               (yMax frame - yMin frame) + yMin frame

-- | Draw a circle with given radius at given point
circle :: MonadPS m => Point -> Double -> m ()
circle (x, y) radius = commL [showt x, showt y, showt radius,
                              "0 360 arc closepath"]

-- | Draw a box of given width and height with (x, y) as bottom left point
box :: MonadPS m => Double -> Double -> Point -> m ()
box w h (x, y) = commL [showt w, showt h,
                        showt x, showt y, "box"]

-- | Doesn't do anything inside Haskell, but makes sure the maximum string
--   width of the input strings with current drawing settings is left on
--   the postscript stack.
getMaxStringWidth :: MonadPS m => [String] -> m ()
getMaxStringWidth ss' = do
    commL [s, "stringwidth pop"]
    mapM_ (\x -> commL [x, "stringwidth pop max"]) ss
  where (s:ss) = map mkPSText ss'

-- | Convert a String to something that can be shown as text inside PS
mkPSText :: String -> T.Text
mkPSText s = T.pack $ "(" ++ s ++ ")"

-- | Generate footer PS code
footer :: MonadPS m => m ()
footer = comm "showpage" >> comm "%%EOF"

