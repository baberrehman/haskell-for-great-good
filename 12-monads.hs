
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Pole
landLeft x (l, r) = (l+x,r)

landRight :: Birds -> Pole -> Pole
landRight x (l, r) = (l,r+x)

landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' x (l,r)
  | abs ((l + x) - r) < 4 = Just (l+x, r)
  | otherwise             = Nothing


landRight' :: Birds -> Pole -> Maybe Pole
landRight' x (l,r)
  | abs ((r + x) - l) < 4 = Just (l, r+x)
  | otherwise             = Nothing

marySue :: Maybe Bool
marySue = do
          x <- Just 9
          Just (x > 8)