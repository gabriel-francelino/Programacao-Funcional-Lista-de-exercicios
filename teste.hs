quadrado100::Int
quadrado100 = sum [ a^2 | a <- [1 .. 100]]

decInt::Int->[Int]
decInt 0 = []
decInt n = n : decInt (n-1)

replica::Int->a->[a]
replica n a = [ a | y<- decInt n, y/=0]

dec2Int::Int->[Int]
dec2Int 1 = [1]
dec2Int n = dec2Int (n-1) ++ [n]

pyths::Int->[(Int,Int,Int)]
pyths n = [(x,y,z) | x<-dec2Int n, y<-dec2Int n, z<-dec2Int n, x^2+y^2==z^2]


factors::Int->[Int]
factors n = [ i | i<-decInt (n-1), mod n i == 0]

perfects::Int->[Int]
perfects n = [ x | x<-dec2Int n, sum (factors x)==x]

positions::Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0 .. n])
	where n = (length xs) - 1

find n l = [b | (a,b)<-l, n==a]

scalarproduct::[Int]->[Int]->Int
scalarproduct xs ys = sum [ x*y | (x,y) <- zip xs ys]


(&!)::Int->Int->Int
(&!) a b
    |b==0 = 1
    |a>0 && b>0 = a * (&!) a (b-1)
    |otherwise = 0

(&!) a b = [a*x | y<-decInt b, x]