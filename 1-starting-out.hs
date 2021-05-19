-- Learn You a Haskell for Great Good!
-- Chapter 1

doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleUs' x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

boomBangs' xs = [ x | x <- xs, x /= 13, x /=15, x /=19]

length' xs = sum [ 1 | _ <- xs ]

removeNonUpperCase xs = [ x | x <- xs, x `elem` ['A'..'Z']]

triangles = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10]]

rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], c^2 == a^2 + b^2]

rightTriangles' = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], c^2 == a^2 + b^2, a + b + c == 24]
