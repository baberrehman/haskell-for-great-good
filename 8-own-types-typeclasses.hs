
data Bool = False | True

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 -y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

data Person = Person String String Int Float String String deriving (Show)


firstName :: Person -> String
firstName (Person fName _ _ _ _ _) = fName

lastName :: Person -> String
lastName (Person _ lName _ _ _ _) = lName


age :: Person -> Int
age (Person _ _ a _ _ _) = a


height :: Person -> Float
height (Person _ _ _ h _ _) = h


phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ p _) = p


flavor :: Person -> String
flavor (Person _ _ _ _ _ i) = i

data Person1 = Person1 { firstName1 :: String,
                          lastName1 :: String,
                          age1 :: Int,
                          height1 :: Float,
                          phoneNumber1 :: String,
                          flavor1 :: String
                        } deriving (Show)


data Car = Car String String Int deriving (Show)

data Car1 = Car1 { company :: String, model :: String, year :: Int } deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = (i*l)+(j*m)+(k*n)