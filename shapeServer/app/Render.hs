module Render(Window,defaultWindow,samples,render) where
import Ansi
import Shapes

import Codec.Picture

--  A window specifies what part of the world to render and at which
--  resolution.
--  Values are top left & bottom right corner to be rendered, 
--             and the size of the output device to render into
data Window = Window Point Point (Int,Int)

-- sets what slice of a drawing will be displayed in the window
-- the 2 points are the bounding box in the drawing of what is being drawn
-- the tuple of integers are the region of the terminal window used to do that drawing
makeWindow :: Point -> Point -> (Int,Int) -> Window
makeWindow p0 p1 size = Window p0 p1 size

-- Default window renders a small region around the origin into
-- a 50x50 character image
defaultWindow :: Window
defaultWindow = Window (point (-1.5) (-1.5)) (point 1.5 1.5) (50,50)


-- Generate a list of evenly spaced samples between two values.
-- e.g. samples -1.5 +1.5 25 generates 25 samples evenly spaced
--      between the two bounds: [ -1.5, -1.375, -1.25 ... ]
samples :: Double -> Double -> Int -> [Double]
samples c0 c1 n = take n [ c0, c0 + (c1-c0) / (fromIntegral $ n-1) .. ]

-- Generate a single sample at the given position
singleSample :: Double -> Double -> Int -> Int -> Double
singleSample c0 c1 n i = c0 + ((c1 - c0) / (fromIntegral $ n-1) * fromIntegral i)

-- Generate the matrix of points corresponding to the pixels of a window.
-- Give us a 2D list of all the available character coordinate positions in the terminal, i.e. all the possble coordinates, i.e. all the pixels
pixels :: Window -> [[Point]]
pixels (Window p0 p1 (w,h)) =
  [ [ point x y | x <- samples (getX p0) (getX p1) w ]
                | y <- reverse $ samples (getY p0) (getY p1) h
  ] 

-- Generate the correpsonding Point to given Window's pixel
singlePixel :: Window -> (Int, Int) -> Point
singlePixel (Window topLeft bottomRight (w, h)) (i, j) = point (singleSample (getX topLeft) (getX bottomRight) w i) (singleSample (getY topLeft) (getY bottomRight) h j)

-- generate list of all screen coordinates in window
coords :: Window -> [[(Int,Int)]]
coords (Window _ _ (w,h)) = [ [(x,y) | x <- [0..w]] | y <- [0..h] ]

-- render a drawing (image/shape) into a given window
render :: Window -> Drawing -> IO ()
render window shape =  sequence_ $ map pix locations     -- we map our pixel testing function over our list of locations that constitute the shape, sequence_ joins all monadic actions together, so it's appplication here means that it generates a list of IO actions which are then all performed one after the other and draw the shape
  where 
    pix (p,(x,y)) | p `inside` shape = goto x y  >> putStr "*"     -- testing a position on the drawing plane (p) to see if it is inside the shape we are drawing. If it is then go to the corresponding (x,y) coordinates in the terminal window and fill the pixel
                  | otherwise     = return ()                   -- if p is not inside the shape we're drawing, do nothing
    -- locations is a list of abstract coords ("pixels") and
    -- corresponding screen coords
    locations :: [ (Point, (Int,Int) ) ]
    locations = concat $ zipWith zip (pixels window) (coords window)    -- taking all the coordinates of the possible places we could pace characters, generating coordinates on the abstract drawing plane for each one
                                                                  -- AKA creating a list of pairs where the first element of each pair is some location in the drawing and the second element is the corresponding coordinate on the screen


renderShape :: String -> Window -> Drawing -> IO()
renderShape path window shape = writePng path $ generateImage pixRenderer widthInPixels heightInPixels
  where
    Window _ _ (widthInPixels, heightInPixels) = window
    pixRenderer x y | pix `inside` shape = PixelRGB8 255 255 255
                    | otherwise = PixelRGB8 0 0 0
        where pix = singlePixel window (x,y)
