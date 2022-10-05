import Data.Char

-- Ex 1 --------
-- a)
f1::Int->Int
f1 x
	|x>=0 = ((x+4) `div` (x+2))
	|otherwise = div 2 x

-- b)
f2::Int->Int->Int
f2 x y
	|x>=y = x+y
	|otherwise = x-y
	
-- c)
f3::Int->Int->Int->Int
f3 x y z
	|(x+y) > z = x+y+z
	|(x+y) < z = x-y-z
	|otherwise = 0 

-- Ex 2 --------
fat::Int->Int
fat 0 = 1 --linha corrigida
fat x = x * fat(x-1)
-- a função não tinha ponto de parada para a recursão

-- Ex 3 --------
soma::Int->Int->Int
soma x y = x+y

multiplica::Int->Int->Int
multiplica a b
	|a==0 || b==0 = 0
	|a<0 && b<0 = (soma (-a) 0) + multiplica (-a) ((-b)-1)
	|a>0 = (soma 0 b) + multiplica (a-1) b
	|b>0 = (soma a 0) + multiplica a (b-1)
	
-- Ex 4 --------
invertInt::Int->Int
invertInt x = read (inverteNum (show x))

inverteNum::String->String
inverteNum [a] = [a]
inverteNum (a:b) = inverteNum b ++ [a] 

-- Ex 5 --------
square :: Int->Int
square num = num*num

fourPower::Int->Int
fourPower x = square x * square x

-- Ex 6 --------
raiz6::Int->Float
raiz6 0 = sqrt (6)
raiz6 x = sqrt (6 +(raiz6 (x-1)))

-- Ex 7 --------
--https://www.todamateria.com.br/analise-combinatoria/#:~:text=sentarem%20neste%20banco.-,Combina%C3%A7%C3%B5es,-As%20combina%C3%A7%C3%B5es%20s%C3%A3o
combinacoes::Int->Int->Int
combinacoes m n = (fat m) `div` ((fat n)*(fat (m-n))) --m>=n

-- Ex 8 --------
mdc::Int->Int->Int
mdc m n 
	|mod m n == 0 = n
	|otherwise = mdc n (mod aux n)
	where
		aux = m
		
-- Ex 9 --------
howManyMultiples::Int->Int->Int->Int
howManyMultiples n a b
	|b<a = 0
	|(mod b n) == 0 = 1 + (howManyMultiples n a (b-1))
	|otherwise = howManyMultiples n a (b-1)

-- Ex 10 -------
lastDigit::Int->Int
lastDigit x = (read (lastDigit1 (show x)))

lastDigit1::[Char]->[Char]
lastDigit1 [a] = [a]
lastDigit1 (a:b) = lastDigit1 b

-- Ex 11 -------
anyDigit::Int->Int->Int
anyDigit x y = (read (anyDigit1 (x) (show y)))

anyDigit1::Int->[Char]->[Char]
anyDigit1 0 (a:b) = [a]
anyDigit1 x (a:b) = anyDigit1 (x-1) b
anyDigit1 x y
	|(x > length y) || (x < 0) = "-1"

-- Ex 12 -------
--allDifferent::Int->Int->Int->Bool
--allDifferent m n p = (m/=n) && (n/=p)
 -- a) não compara o valor de m com para
allDifferent::Int->Int->Int->Bool
allDifferent m n p = (m/=n) && (n/=p) && (m/=p)

-- Ex 13 -------
howManyEqual::Int->Int->Int->Int
howManyEqual a b c 
	| (a==b) && (a==c) = 3
	| (a/=b) && (b/=c) && (a/=c) = 0
	| otherwise = 2

-- Ex 14 -------
periodo::Int
periodo = 7

-- tabela de vendas
vendas :: Int -> Int
vendas 1 = 41
vendas 2 = 72
vendas 3 = 41
vendas 4 = 2
vendas 5 = 91
vendas 6 = 55
vendas 7 = 41
vendas _ = 0

-- a){-parameters: value; interval beginning; interval ending; return value-}
howManyLess::Int->Int->Int->Int
howManyLess _ _ 0 = 0
howManyLess v i f
	|(vendas f) < v && f>=i = 1 + howManyLess v i (f-1)
	|otherwise = howManyLess v i (f-1)

-- b)
noZeroInPeriod::Int->Bool
noZeroInPeriod p = ((howManyLess 1 1 p) == 0)

-- c)
zerosInPeriod::Int->[Int]
zerosInPeriod periodo
	|periodo>0 && (vendas periodo == 0) = periodo : zerosInPeriod (periodo - 1)
	|otherwise = []
	
-- d)
listVendasBaixo::Int->Int->[Int]
listVendasBaixo _ 0 = []
listVendasBaixo v p
	|(vendas p < v) = p : (listVendasBaixo v (p-1))
	|otherwise = listVendasBaixo v (p-1)
	
-- 15 ---------
fib :: Int -> Int
fib n
	| n == 0 = 0
	| n == 1 = 1
	| n > 1 = fib (n-2) + fib (n-1)
	
-- lista que vai guardar a sequencia
listFib::Int->[Int]
listFib 0 = [0]
listFib n = (listFib (n-1)) ++ [fib n]

-- interface para antFib1 
antFib::Int->Int
antFib n = antFib1 n 0 (listFib n)

antFib1::Int->Int->[Int]->Int
antFib1 _ _ [] = -1
antFib1 n i (a:b)
	|n /= a = (antFib1 n (i+1) b)
	|n == a = i

-- 16 ---------
funny x y z
	| x> z = True
	|y >= x  = False
	|otherwise = True

funny3 x y z = not(y>=x)

-- 17 ---------
uppercase :: Char->Char
uppercase a
	|(isLower a) = (toUpper a)
	|otherwise = a

-- 18 ----------
charToNum::Char->Int
charToNum a
	|isDigit a = 64-(ord a)
	|otherwise = -1

-- 19 ----------
duplicate::String->Int->String
duplicate _ 0 = ""
duplicate a n = a ++ duplicate a (n-1)

-- 20 ----------
pushRight::String->Int->String
pushRight s n
	|n < (length s)+1 = s
	|otherwise = '>' : pushRight s (n-1)

-- 21 ----------
(&-)::Int->Int->Int
(&-) x y = x - 2*y

{- 
*)  0 a)-8 b)-6 c)-4 R:Dependendo da ordem de precedencia das operações os resultados podem mudar
**) 8 a)0 b)4
-}

-- 22 ---------
inverte :: [Int]->[Int]
inverte [a] = [a]
inverte (a:b) = (inverte b)++[a]

-- 23 ---------
par::[Int]->[Int]
par [] = []
par (a:b)
	|mod a 2 == 0 = a : par b
	|otherwise = par b

impar::[Int]->[Int]
impar [] = []
impar (a:b)
	|mod a 2 /= 0 = a : impar b
	|otherwise = impar b

separa::[Int]->([Int],[Int])
separa a = (par a, impar a)

-- 24 ----------
converte::[Int]->[Char]
converte [] = ""
converte (a:b) = chr (a+64) : converte b

-- 25 ----------
{-
(a) ['a'..'g'] = "abcdefg"
(b) [0.1 ..0.9] = [0.1,1.1]
(c) [0.1,0.3 .. 0.9] = [0.1,0.3,0.5,0.7,0.9]
(d) [0.1,0.3 ..1.8] = [0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9]
(e) [0.4,0.2 ..0.8] = []
(f) [1,4..15] = [1,4,7,10,13]
-}

-- 26 ----------
contaChar::[Char]->String->Int
contaChar [] _ = 0
contaChar (a:b) c
	|c == [a] = 1+ contaChar b c
	|otherwise = contaChar b c

-- 27 ----------
contaInt::[Int]->Int->Int
contaInt [] _ = 0
contaInt (a:b) c
	|c == a = 1 + contaInt b c
	|otherwise = contaInt b c

purifica::[Int]->[Int]
purifica [] = []
purifica (a:b)
	|contaInt (a:b) a == 1 = a : purifica b
	|otherwise = purifica b

-- 28 ----------
repete _ 0 = []
repete a i = a : repete a (i-1)

proliferaInt::[Int]->[Int]
proliferaInt [] = []
proliferaInt (a:b) = (repete a a) ++ proliferaInt b

-- 29 ----------
proliferaChar::[Char]->String
proliferaChar "" = ""
proliferaChar (a:b) = (repete a ((ord a)-64)) ++ proliferaChar b