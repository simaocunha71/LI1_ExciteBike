{- |
= Introdução
Na tarefa 1, tivemos o objetivo de gerar um mapa de forma aleatória.

= Objetivos

O nosso principal objetivo era criar um mapa de forma aleatória dando-lhe 3 inputs (numero de pistas, comprimento de cada pista - numero de peças e um numero aleatorio).

Para tal, os professores da cadeira forneceram-nos a função "geraAleatorios", que nos permitiu implementar na função principal "gera" um numero aleatorio de 0 a 9.

Para isto temos que ter a noção que o mapa é uma grelha de pistas, isto é, uma matriz de pistas.

= Discussão e conclusão

A criação de cada peça foi de acordo com o par de aleatorios existentes na matriz. 
Então, tal como está explicito no enunciado da 1ºfase, a peça será criada de acordo com um gama para determinar se é rampa a subir, rampa a descer ou reta, e o outro gama irá determinar o piso da peça criada.

As pistas foram geradas de forma recursiva, sendo a sua primeira peça (Recta Terra 0). De seguida, estas pistas são introduzidas numa matriz através da função "matrizParaMapa".

Na função "gera" existem duas alternativas: ou o mapa tem 1 só pista ou tem mais do que uma pista. 
Neste primeiro caso, a primeira peça é (Recta Terra 0) e, de forma recursiva, vai diminuindo o numero de pistas até que entre no caso de paragem.
Já no segundo caso, todas as pistas são alinhadas sobre a forma de matriz.

Esta tarefa correu bem, uma vez que todos os nossos testes foram aprovados pelo o oráculo dos professores e a cobertura dos nossos testes com o nosso código é de 100%.

Para concluir, consideramos que esta tarefa foi interessante para uma primeira abordagem para o projeto de Laboratorios de Informática I, pois obrigou-nos a trabalhar muito e, assim, entrar no ritmo ao qual este curso exige.

-}


module Tarefa1_2019li1g101 where

import LI11920
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(0,1,0),(0,1,1),(1,1,1),(2,2,0),(2,5,1),(2,7,3),(2,2,2),(7,3,7),(3,2,0),(0,3,0),(1,23,9),(23,1,8),(0,36,8),(16,16,16),(7,1,8)]


-- * Funções principais da Tarefa 1.
-- | Função principal que vai gerar o mapa 
gera :: Int -- ^ Representa o número de pistas a gerar
     -> Int -- ^ Representa o comprimento de cada pista
     -> Int -- ^ Representa o número aleatório (seed) para a formação de um mapa
     -> Mapa -- ^ Representa o mapa desejado da Tarefa1_2019li1g101
gera 0 a b = []
gera npistas ncolunas seed = if ncolunas == 1 && npistas > 0 then [Recta Terra 0] : gera (npistas-1) ncolunas seed 
                           else m     
    where
    xs = geraAleatorios(((ncolunas-1)*(2*npistas))*1) seed
    ps = aleatoriosParaPar xs
    ms = parParaMatriz ncolunas ps
    m = matrizParaMapa ncolunas ms

-- * Funções pré-definidas da Tarefa 1.
----------------------------------------------------------------
-- | Função que vai gerar um número aleatório para formar um mapa 
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))
----------------------------------------------------------------
-- | Função que transforma a lista de números aleatórios numa lista de pares (futuras peças)
aleatoriosParaPar :: [Int] -- ^ Representa a lista de inteiros aleatórios gerados pela funçãpo geraAleatorios
                     -> [(Int,Int)] -- ^ Representa a lista de pares de Inteiros formados a partir da lista de inteiros  aleatórios
aleatoriosParaPar [] = []
aleatoriosParaPar (h:hs:z) = (h,hs) : aleatoriosParaPar z

-- | Função que pega na lista e pares aleatorios e mete esses pares numa matriz que representa o espaço do mapa final

parParaMatriz :: Int -- ^ Representa o numero de colunas que queremos que a matriz tenha 
                 -> [a] -- ^ Representa a Lista de pares de inteiros 
                 -> [[a]] -- ^ Representa a Matriz formada pelos pares de inteiros com o numero de colunas dado

parParaMatriz a [] = []
parParaMatriz n l = (take (n-1) l):(parParaMatriz n (drop (n-1) l))

-- | Função que pega nos pares da matriz e transforma esses pares no mapa a criar

matrizParaMapa :: Int -- ^ Representa o numero de colunas
                  -> [[(Int,Int)]] -- ^ Representa a Matriz de pares de inteiros aleatórios
                  -> Mapa -- ^ Representa o mapa que é gerado aplicando uma função a cada elemento da matriz

matrizParaMapa a [] = [[Recta Terra 0]]           
matrizParaMapa a l = map (geraPista a) l

-- | Função que gera uma pista 

geraPista :: Int -- ^ Representa o comprimento da pista
            -> [(Int,Int)] -- ^ Representa um elemento da matriz 
            -> Pista -- ^ Gera a pista desejada, adicionando o primeiro elemento, que é igual em todas as pistas, no inicio da mesma   

geraPista ncolunas as = (Recta Terra 0): geraPistaAux (ncolunas-1) as (Recta Terra 0)

-- | Função auxiliar a utilizar em "geraPista" 

geraPistaAux :: Int -- ^ Representa o comprimento da pista 
                -> [(Int,Int)] -- ^ Representa um elemento da matriz (lista de inteiros aleatorios que representa uma pista)
                -> Peca -- ^ Representa a peça anterior
                -> Pista -- ^ Representa a Pista gerada pela linha fornecida mas não tem o primeiro elemento 

geraPistaAux 0 as pa = []
geraPistaAux (ncolunas) as pa = (constroiPeca (head as) pa): geraPistaAux (ncolunas-1) (tail as)(constroiPeca (head as) pa)

-- | Nesta função, iremos ter 2 variantes, uma para o caso de a peça ser do tipo Recta Piso Int, e outra para o caso de ser Rampa Piso Int Int
constroiPeca :: (Int,Int) -- ^ Representa o par de números para definir o piso e a altura, respetivamente
                -> Peca -- ^ Representa a peça anterior à desejada
                -> Peca -- ^ Representa a peça desejada

constroiPeca (i1,i2) (Recta a b) |(i2 == 0 || i2 ==1) = Rampa (decidePiso i1 a) b (b + i2 + 1)
                                 |(i2 == 2 || i2 == 3 || i2 == 4 || i2 ==5) =
                                  if (b - (i2 - 1)) < 0 && b == 0 then Recta (decidePiso i1 a) 0
                                  else if (b - (i2 - 1)) < 0 then Rampa (decidePiso i1 a) b 0 
                                  else Rampa (decidePiso i1 a) b (b - (i2 - 1))
                                 |otherwise = Recta (decidePiso i1 a) b 


constroiPeca (i1,i2) (Rampa a b c) |(i2 == 0 || i2 ==1) = Rampa (decidePiso i1 a) c (c + i2 + 1)
                                   |(i2 == 2 || i2 == 3 || i2 == 4 || i2 ==5) = 
                                       if (c - (i2 - 1)) < 0 && c == 0 then Recta (decidePiso i1 a) 0
                                       else if (c - (i2 - 1)) < 0 then Rampa (decidePiso i1 a) c 0 
                                       else Rampa (decidePiso i1 a) c (c - (i2 - 1))
                                   |otherwise = Recta (decidePiso i1 a) c 

-- | Função que determina o tipo de piso a gerar

decidePiso :: Int -- ^ Representa o gama que levará ao piso a formar
              -> Piso -- ^ Representa o piso da peça anterior 
              -> Piso -- ^ Representa o piso desejado

decidePiso a b | (a == 0 || a ==1) = Terra
               | (a == 2 || a ==3) = Relva
               | a == 4 = Lama
               | a == 5 = Boost
               | otherwise = b
