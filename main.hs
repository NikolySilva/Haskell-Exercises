--Exercícios Haskell

-- 2.1) Gere as listas
--a)[1,11,121,1331,14641,161051,1771561]	

lista2A :: [Int]
lista2A = [11 ^ x | x<-[0 .. 6]]

--b) [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,33,34,35,37,38,39]	

lista2B :: [Int]
lista2B = [x | x <- [1..40], mod x 4/=0]

--c)["AaBB",	 "AbBB",	 "AcBB",	 "AdBB",	 "AeBB",	 "AfBB","AgBB"]	

lista2C :: [String]
lista2C = [[a] ++ [xs] ++ "BB" | a <- ['A'], xs <- ['a'..'g']]

--d)[5,8,11,17,20,26,29,32,38,41]

lista2D :: [Int]
lista2D = [3*x+2| x <- [1..13], x/=4, x/=7, x/=11]

--e)[1.0,0.5,0.25,0.125,0.0625,0.03125]	

lista2E ::[Float]
lista2E = [1/(2^x)| x <- [0..5]]

--f)[1,10,19,28,37,46,55,64]	

lista2F :: [Int]
lista2F = [9*x + 1 | x <- [0..7]]

--g)[2,4,8,10,12,16,18,22,24,28,30]	

lista2G :: [Int]
lista2G = [x*2 | x <- [1..15], x*2 `notElem` [6,14,20,26]]

--h)		['@','A','C','D','E','G','J','L']

lista2H :: [Char]
lista2H = [x | x <- '@' : ['A'..'L'], x `notElem` ['B','F','H','I','K']]

--2.2)Crie uma função que verifique se o tamanho de	uma String é par ou	não. Use Bool como retorno.


--2.3) Escreva uma função que receba um vetor de Strings e retorne	uma	lista	com	todos	os	elementos	em	ordem	reversa


