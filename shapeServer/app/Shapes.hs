module Shapes(
  Shape, Point, Vector, Transform, Drawing,
  point, getX, getY,
  empty, circle, square,
  identity, translate, rotate, scale, (<+>),
  inside)  where


-- Utilities

data Vector = Vector Double Double
              deriving Show
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

getX :: Vector -> Double
getX (Vector x y) = x

getY :: Vector -> Double
getY (Vector x y) = y
        
-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector


data Shape = Empty 
           | Circle 
           | Square
           | Polygon [Point]
             deriving Show

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square
polygon ps = Polygon ps

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Matrix
             deriving Show

identity = Identity
translate = Translate
scale = Scale
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity x                   = id x                 -- use this when creating a (not polygon) shape to pass in the point
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = (invert m) `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p


-- Drawings

type Drawing = [(Transform,Shape)]


-- interpretation function for drawings

inside :: Point -> Drawing -> Bool
inside p d = or $ map (inside1 p) d

inside1 :: Point -> (Transform, Shape) -> Bool
inside1 p (t,s) = insides (transform t p) s

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1
p `insides` Polygon ps = insidePolygon p (head ps) ps False

{-  Algorithm in C code (https://wrf.ecse.rpi.edu/Research/Short_Notes/pnpoly.html)
  int pnpoly(int nvert, float *vertx, float *verty, float testx, float testy)
  {
    int i, j, c = 0;
    for (i = 0, j = nvert-1; i < nvert; j = i++) {
      if ( ((verty[i]>testy) != (verty[j]>testy)) &&
      (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
        c = !c;
    }
    return c;
  }
-}


insidePolygon :: Point -> Point -> [Point] -> Bool -> Bool
-- for a list that only has 1 or 0 items left (the iterations have finished and there are no more points to check OR the user passed in an empty shape)
insidePolygon _ _ points c | length points <= 1 = c
-- for the first iteration where the ith point is the first point in the list and c is 0 (so false)
insidePolygon testPoint ithPoint (p:q:ps) False | (getX ithPoint == getX p) && (getY ithPoint == getY p) = do
        if ((getY p > getY testPoint) /= (getY jthPoint > getY testPoint)) && (getX testPoint < ((getX jthPoint - getX p) * (getY testPoint - getY p) / ( getY jthPoint - getY p) + getX p))
            then insidePolygon testPoint q (p:q:ps) (not False)
        else insidePolygon testPoint q (p:q:ps) False
        where jthPoint = last ps
-- for any iteration after i = 0 (so i = 1 and j = 0 which means that ithPoint = q and jthPoint = p)
insidePolygon testPoint _ (p:q:ps) c = do
          if (((getY q) > (getY testPoint)) /= ((getY p) > (getY testPoint))) && ((getX testPoint) < (((getX p)-(getX q)) * ((getY testPoint)-(getY q)) / ((getY p)-(getY q)) + (getX q)))
            then insidePolygon testPoint q (q:ps) (not c)
          else insidePolygon testPoint q (q:ps) c
-- this line is just here to get rid of annoying "non-exhaustive pattern matches" warning
insidePolygon _ _ _ _ = False

distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)

testShape = (scale (point 10 10), circle)
