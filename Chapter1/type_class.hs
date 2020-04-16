data RGB = RGB Int Int Int
    deriving Eq

-- instance Eq RGB where
--   (RGB r1 g1 b1) == (RGB r2 g2 b2) =
--     (r1 == r2) && (g1 == g2) && (b1 == b2)

instance Show RGB where
  show (RGB r g b) =
    "RGB" ++ (show r) ++ " " ++ (show g) ++ ", " ++ (show b)

colors =
  [ RGB 255 0 0
  , RGB 0 255 0
  , RGB 0 0 255
  ]

green = RGB 0 255 0
greenInColors = elem green colors

---

data Maybe' a = Nothing'
    | Just' a

instance (Eq a) => Eq (Maybe' a) where
  Nothing' == Nothing' = True
  Nothing' == (Just' _) = False
  (Just' _) == Nothing' = False
  (Just' x) == (Just' y) = x == y

class Measurable a where
  distance :: a -> a -> Double

data Point2 = Point2 Double Double
    deriving Show

data Point3 = Point3 Double Double Double
    deriving Show

distance2 :: Point2 -> Point2 -> Double
distance2 (Point2 x1 y1) (Point2 x2 y2) =
  sqrt (dx * dx + dy * dy)
  where dx = x1 - x2
        dy = y1 - y2

distance3 :: Point3 -> Point3 -> Double
distance3 (Point3 x1 y1 z1) (Point3 x2 y2 z2) =
  sqrt (dx * dx + dy * dy + dz * dz)
  where dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2

instance Measurable Point2 where
  distance = distance2

instance Measurable Point3 where
  distance = distance3

instance Measurable Double where
  distance x y = abs (x - y)

pathLength :: Measurable a => [a] -> Double
pathLength []       = 0
pathLength (_ : []) = 0
pathLength (p0 : p1 : rest) =
  distance p0 p1 + pathLength (p1 : rest)

class (Measurable a, Show a) => Directions a where
  getDirections :: a -> a -> String
  getDirections p1 p2 =
    "Go from " ++ (show p1) ++
    " towards" ++ (show p2) ++
    " and stop after" ++ (show $ distance p1 p2)

instance Directions Point2 where

instance Directions Point3 where
  getDirections p1 p2 =
    "Fly from " ++ (show p1) ++
    " towards " ++ (show p2) ++
    " and stop after " ++ (show $ distance p1 p2)
