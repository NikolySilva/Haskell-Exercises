
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

ehPar :: String -> Bool
ehPar x = even (length x)

--2.3) Escreva uma função que receba um vetor de Strings e retorne	uma	lista	com	todos	os	elementos	em	ordem	reversa

reverseString :: [String] -> [String]
reverseString x = reverse x

-- 2.4)	 Escreva uma função que	 receba	 um	 vetor	 de	 Strings e retorne	 uma	lista	 com	 o	 tamanho	 de	 cada	 String.	As	palavras	 de tamanho	par	devem	ser	excluídas	da	resposta.

lthRvrsString :: [String] -> [Int]
lthRvrsString xs = [length x | x <- xs]

--2.5)	Escreva	a	função		head		como	composição	de	duas	outras. (Entre com um array de strings, pegue a prieira posição, inverta a string e retorne a primeira letra)

headComp :: [String] -> Char
headComp xs = head $ reverse $ head[x | x <- xs]

---2.6)	Faça	uma	função	que	receba	uma	String	e	retorne		True	se	esta	for	um	palíndromo;	caso	contrário,		False	.

palinStr :: String -> Bool
palinStr x = reverse x == x

--2.7	 Faça	 uma	 função	 que	 receba	 um	 inteiro	 e	 retorne	 uma tupla,	contendo:	o	dobro	deste	número	na	primeira	coordenada,	o triplo	na	segunda,	o	quádruplo	na	terceira	e	o	quíntuplo	na	quarta.

funcao7 :: Int -> (Int, Int, Int, Int)
funcao7 x = let [a, b, c, d] = [x * y | y <- [2 .. 5]] in (a, b, c, d)

------------------------------------------------------------------------------------------------------------------------------

-- 3.1 Crie o tipo Pergunta com os values constructors Sim ou Nao . Faça as funções seguintes, determinando seus tipos explicitamente.
--Resposta:

data Pergunta = Sim | Nao deriving (Show)

pergNum :: Pergunta -> Int
pergNum Nao = 0
pergNum Sim = 1

listPergs :: [Pergunta] -> [Int]
listPergs xs = [pergNum x | x <- xs]

and' :: Pergunta -> Pergunta -> Bool
and' Sim Sim = True
and' _ _ = False

or' :: Pergunta -> Pergunta -> Bool
or' Nao Nao = False
or' _ _ = True

not' :: Pergunta -> Bool
not' Sim = False
not' Nao = True

--3.3 Implemente uma função que simule o vencedor	de uma partida de pedra, papel e tesoura usando	tipos criados. Casos de empate devem ser considerados em seu tipo.
--Resposta:

data Jogada = Pedra | Papel | Tesoura deriving (Show)

partida :: Jogada -> Jogada -> String
partida Pedra Pedra = "Empate"
partida Papel Papel = "Empate"
partida Tesoura Tesoura = "Empate"
partida Pedra Tesoura = "Pedra venceu!"
partida Tesoura Pedra = "Pedra venceu!"
partida Papel Pedra = "Papel venceu!"
partida Pedra Papel = "Papel venceu!"
partida Tesoura Papel = "Tesoura venceu!"
partida Papel Tesoura = "Tesoura venceu!"

--3.4 Faça uma função que retorne uma string, com todas as vogais maiúsculas e minúsculas eliminadas de uma string passada por parâmetro usando list compreenshion.
--Resposta: 

vogais :: String
vogais = "aeiouAEIOU"

removeVogais :: String -> String
removeVogais palavra = [x | x <- palavra, x `notElem` vogais]


-- 3.7 Faça uma função	que	receba uma String e retorne True se esta for um palíndromo; caso contrário, False.
--Resposta:

isPalindromo :: String -> Bool
isPalindromo str = str == reverse str

--3.11 Crie o tipo	de dado Binario que pode ser Zero ou Um. Faça outro tipo de dado chamado Funcao que pode ser Soma2, Maior, Menor ou Mult2. Implemente a função aplicar que recebe uma Funcao e dois Binarios. Seu retorno consiste em executar a operação desejada
--Resposta:

data Binario = Zero | Um deriving (Show, Eq)

data Funcao = Soma2 | Maior | Menor | Mult2 deriving (Show, Eq)

aplicar :: Funcao -> Binario -> Binario -> Binario
aplicar f x y = case f of
    Soma2  -> soma2 x y
    Maior  -> maior x y
    Menor  -> menor x y
    Mult2  -> mult2 x y

soma2 :: Binario -> Binario -> Binario
soma2 Zero Zero = Zero
soma2 Zero Um   = Um
soma2 Um   Zero = Um
soma2 Um   Um   = Zero

maior :: Binario -> Binario -> Binario
maior Zero Zero = Zero
maior Zero Um   = Um
maior Um   Zero = Um
maior Um   Um   = Um

menor :: Binario -> Binario -> Binario
menor Zero Zero = Zero
menor Zero Um   = Zero
menor Um   Zero = Zero
menor Um   Um   = Um

mult2 :: Binario -> Binario -> Binario
mult2 Zero _ = Zero
mult2 _ Zero = Zero
mult2 Um   Um = Um


-- 3.18 Faça uma função encriptarTodos que encripta	(ou	dá erro) todos os elementos	de um vetor	de Cripto
--Resposta:

data Cripto = Cripto String deriving (Show, Eq)

encriptarTodos :: [Cripto] -> [Cripto]
encriptarTodos = map encriptar

--3.21 Crie a função maxMoeda que recebe uma lista de moedas e retorna o valor máximo absoluto (sem conversão alguma) dentre os campos val desta lista. Exemplo:
--Prelude> maxMoeda [Moeda 3 Real, Moeda 7 Dollar, Moeda 1 Euro] 7
--Use a função maximum.

Resposta:

data Unidade = Real | Dollar | Euro deriving (Show)

data Moeda = Moeda {valor :: Int, unidade :: Unidade} deriving (Show)

maxMoeda :: [Moeda] -> Int
maxMoeda moedas = maximum [valor moeda | moeda <- moedas]

