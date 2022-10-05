import Data.Char

-- Ex1 -------------
quadrado100::[Int]
quadrado100 = sum [a^2 | a <- [1 .. 100]]

-- Ex2 -------------
decInt 0 = []
decInt i = i : decInt (i-1)

replicar::Int->a->[a]
replicar 0 _ = []
replicar n b = [ x | x <- [b], i<- decInt n]

replicar2::Int->a->[a]
replicar2 0 _ = []
replicar2 n x = x : replicar2 (n-1) x

-- Ex3 ----------
pyths::Int->[(Int,Int,Int)]
pyths n = [(x,y,z) | x<-[1 .. n], y<-[1 .. n], z<-[1 .. n], x^2+y^2==z^2]

-- Ex4 ----------
{-
factors::Int->[Int]
factors n = factor n (n-1)

factor::Int->Int->[Int]
factor n 0 = []
factor n i
    | n `mod` i == 0 = factor n (i-1) ++ [i]
    |otherwise = factor n (i-1)
-}

factors::Int->[Int]
factors n = [x | i<-decInt (n-1), mod n i] 

perfects::Int->[Int]
perfects n = [x | x <- decInt n, sum (factors x) == x]

-- Ex5 -----------
exemplo5 = [(x,y) | x <- [1,2,3], y <- [4,5,6]]

criaTupla = concat [ [ (x,y) | y <- [4,5,6] ] | x <- [1,2,3]]

-- Ex6 -----------
-- “Para cada tipo a que pertencer a classe Eq, a função positions tem tipo a -> [a] -> [Int].
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n]) 
    where n = (length xs) - 1

find n l = [ b | (a,b) <- l, n==a ]

{- find sem list compreenshion** (não sei como escreve)
find _ [] = []
find n ((a,b):c)
    |n==a = b : find n c
    |otherwise = find n c
-}
--positions (1,1) [(1,1),(2,2),(3,3),(1,1)]
--find 5 (zip [5,4,3,2,1] [0,1,2,3,4])

-- Ex7 ----------
une::[Int]->[Int]->[(Int,Int)]
une [] [] = []
une (a:b) (c:d) = (a,c) : une b d

scalarproduct::[Int]->[Int]->Int
scalarproduct xs ys = sum [ x*y | (x,y) <- une xs ys]

{-
ou poderia usar o zip no lugar do une
scalarproduct::[Int]->[Int]->Int
scalarproduct xs ys = sum [ x*y | (x,y) <- zip xs ys]
-}

-- Ex8 ---------
(&!)::Int->Int->Int
(&!) a b
    |b==0 = 1
    |a>0 && b>0 = a * (&!) a (b-1)
    |otherwise = 0

{- 
2 &! 3
= 2 * (2 &! 2)
 = 2 * 2 * (2 &! 1)
  = 2 * 2 * 2 * ( 2 &! 0)
   = 2 * 2 * 2 * 1
    = 8
-}

-- Ex9 -------------
expression1 f p xs = [f x | x <- xs, p x]

expression::(a->b)->(a->Bool)->[a]->[b]
expression f p xs = map f (filter p xs)

-- Ex10 -------------
dec2Int::[Int]->Int
dec2Int l =  read [intToDigit x | x <- l]

-- Ex11
unfold p h t x
    |p x = []
    |otherwise = h x : unfold p h t (t x)

{-
--teste::(a->Bool)->(a->b)->(a->c)->[a]->[a]
teste f1 f2 calda x
    | f1 x = []
    | otherwise = f2 x : teste f1 f2 calda (calda x)
-}
f01 x = (x == [])

f02 (a:b) = a*3

f03 (a:b) = b