
import qualified Data.Map as Map  


-- data Bool = False | True

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

data Person2 = Person2 {
    firstName2 :: String,
    lastName2  :: String,
    age2       :: Int
} deriving (Eq, Show, Read)


mikeD = Person2 {firstName2 = "Michael", lastName2 = "Diamond", age2 = 43}  

adRock = Person2 {firstName2 = "Adam", lastName2 = "Horovitz", age2 = 41}  

mca = Person2 {firstName2 = "Adam", lastName2 = "Yauch", age2 = 44}  

mikeD1 = Person2 {firstName2 = "Michael", lastName2 = "Diamond", age2 = 43}


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)


-- Types

  
data LockerState = Taken | Free deriving (Show, Eq)  
  
type Code = String  
  
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookUp :: Int -> LockerMap -> Either String Code
lockerLookUp lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                              then Right code
                              else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]


infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys      = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq, Ord)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right)
  | x == y = Node y left right
  | x < y  = Node y (treeInsert x left) right
  | x > y  = Node y left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node y left right)
  | x == y = True
  | x < y  = treeElem x left
  | x > y  = treeElem x right