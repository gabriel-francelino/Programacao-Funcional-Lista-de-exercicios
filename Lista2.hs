import Data.Char

-- Ex 1 ---------
converte::Char->(Char,Char,Int)
converte b
	|isLower b = (b,(toUpper b),(ord b))
	|otherwise = (b,(toLower b),(ord b))
	
-- Ex 2 ---------
type Pessoa = (String, Int, Char)

pessoa::Int->Pessoa
pessoa rg
	|rg==1 = ("Joao Silva", 12, 'm')
	|rg==2 = ("Jonas Souza", 51, 'm')
	|rg==321 = ("Jocileide", 21, 'f')
	|otherwise = ("Nao ha ninguem", 9999, 'x')
-------------------------------
nome (n,_,_) = n	
idade (_,i,_) = i
sexo (_,_,s) = s
-------------------------------
ordenacao::[Int]->[Int]
ordenacao [] = []
ordenacao (a:b) = insere a (ordenacao b)

insere::Int->[Int]->[Int]
insere x [] = [x]
insere x (a:b)
	|x<=a = x:(a:b)
	|otherwise = a:insere x b
-------------------------------

-- a)
listIdade::Int->[Int]
listIdade 0 = []
listIdade n
	|idade (pessoa n) /= 9999  = listIdade (n-1) ++ [idade (pessoa n)]
	|otherwise = listIdade (n-1)

pessoaMenorIdade::Int->String
pessoaMenorIdade 0 = ""
pessoaMenorIdade rg
	|idade (pessoa rg) == menorIdade (ordenacao (listIdade rg)) = nome (pessoa rg)
	|otherwise = pessoaMenorIdade (rg-1)
	
menorIdade::[Int]->Int
menorIdade (a:b) = a

maiorIdade::[Int]->Int
maiorIdade [a] = a
maiorIdade (a:b) = maiorIdade (ordenacao b)

-- b)
somaIdades::[Int]->Float
somaIdades [] = 0
somaIdades (a:b) = fromIntegral a + somaIdades b

quantidadeRG::[Int]->Float
quantidadeRG [] = 0
quantidadeRG (a:b) = 1 + quantidadeRG b

mediaIdadesRG::Int->Float
mediaIdadesRG rg = somaIdades x / quantidadeRG x
	where
		x = (listIdade rg)

-- c)
masculinoRG::Int->Int
masculinoRG 0 = 0
masculinoRG rg
	|sexo (pessoa rg) == 'm' = 1 + masculinoRG (rg-1)
	|otherwise = masculinoRG (rg-1)
	
-- d)
maiorIdadeRG::Int->Int
maiorIdadeRG rg
	|idade (pessoa rg) == maiorIdade (listIdade rg) = rg
	|otherwise = maiorIdadeRG (rg-1)	
-- Ex 3 ---------
--mesmo do Ex1

-- Ex 4 ---------
type T_Int4 = (Int,Int,Int,Int)

insTupOrd::[Int]->T_Int4
insTupOrd (a:b:c:[d]) = (a,b,c,d)

ordena::Int->Int->Int->Int->T_Int4
ordena a b c d = insTupOrd (ordenacao (a:b:c:[d]))

-- Ex 5 ---------
type Data = (Int,Int,Int)

converteDias::Data->Int
converteDias (d,m,a) 
	|(mod a 4) == 0 = d + (m*30) + (a*366)
	|otherwise = d + (m*30) + (a*365)

diasEntreDatas::Data->Data->Int
diasEntreDatas d1 d2 = (converteDias d2) - (converteDias d1)

-- Ex 6 ---------
delta::(Int,Int,Int)->Int
delta (a,b,c) = b^2 - 4*a*c

raiz1::(Int,Int,Int)->Int->Float
raiz1 (a,b,c) d = ((fromIntegral (-b)) + sqrt (fromIntegral (d))) / ( 2 * fromIntegral a)

raiz2::(Int,Int,Int)->Int->Float
raiz2 (a,b,c) d = ((fromIntegral (-b)) - sqrt (fromIntegral (d))) / ( 2 * fromIntegral a)

equacao::(Int,Int,Int)->(Float,Float)
equacao e
	|delta e >=0 = (raiz1 e (delta e),raiz2 e (delta e))
	|otherwise = (9999,9999)
	
-- Ex 7 --------
somaT::(Int,Int,Int)->Int
somaT (a,b,c) = a+b+c

triangulo::(Int,Int,Int)->(String,Int)
triangulo (a,b,c)
	|a==b && a==c = ("Equilatero", somaT (a,b,c))
	|a/=b && b/=c && a/=c = ("Escaleno", somaT (a,b,c))
	|otherwise = ("Isoceles", somaT (a,b,c))
	
-- Ex 8 --------
type DadosBase = (Int,String,String,Char)
base::Int->DadosBase
base x
	|x == 0 = (1793, "Pedro Paulo" ,"MESTRE", 'M')
	|x == 1 = (1797, "Joana Silva Alencar","MESTRE", 'M')
	|x == 2 = (1534, "Joao De Medeiros" ,"DOUTOR", 'F')
	|x == 3 = (1267, "Claudio Cesar de Sa","DOUTOR", 'M')
	|x == 4 = (1737, "Paula de Medeiros" ,"MESTRE", 'F')
	|x == 5 = (1888, "Rita de Matos" ,"MESTRE", 'F')
	|x == 9 = (1698, "Tereza Cristina Andrade" ,"MESTRE" , 'F')
	|otherwise = (0,"","",'O')
	
-- a)
numDoutoresBase::Int
numDoutoresBase = numDoutores (base 10) 10

numDoutores::DadosBase->Int->Int
numDoutores (a,n,t,s) i
	|(t == "DOUTOR") && (i>=0) = 1 + numDoutores (base (i-1)) (i-1)
	|i < 0 = 0
	|otherwise = numDoutores (base (i-1)) (i-1)

-- b)
numMulheresBase::Int
numMulheresBase = numMulheres (base 10) 10

numMulheres::DadosBase->Int->Int
numMulheres (a,n,t,s) i
	|(s=='F') && (i>=0) = 1 + numMulheres (base (i-1)) (i-1)
	|i < 0 = 0
	|otherwise = numMulheres (base (i-1)) (i-1)

-- c)
numMestresMBase::Int
numMestresMBase = numMestresM (base 10) 10

numMestresM::DadosBase->Int->Int
numMestresM (a,n,t,s) i
	|(t == "MESTRE") && (s == 'M') && (i>=0) = 1 + numMestresM (base (i-1)) (i-1)
	|i < 0 = 0
	|otherwise = numMestresM (base (i-1)) (i-1)

-- d)****
profMaisAntigo::String
profMaisAntigo = maisAntigo (base 10) 10

maisAntigo::DadosBase->Int->String
maisAntigo (a,n,t,s) i
	|i<0 = "Erro"
	|aux>a && i>=0 = n
	|otherwise = maisAntigo (base (i-1)) (i-1)
	where
		aux = a






