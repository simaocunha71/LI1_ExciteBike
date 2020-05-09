{- |
    
    = Introdução
    
    O objectivo desta tarefa é, dado um mapa cujo comprimento de cada pista é maior que 1, convertê-lo numa 
    sequência de instruções a dar a um grupo de bulldozers (um por pista) que avançam da partida para construir 
    o mapa em questão. Iremos também implementar métodos para reduzir e poupar ao maximo o tamanho da lista de 
    intruções que teremos aqui como resultado.
    
    = Objetivos
    
    O objectivo desta tarefa é pegar num mapa normal e transforma-lo numa lista de  intruções que uma suposta máquina 
    terá de seguir para "alisar" o pavimento. Por isso começamos por transformar cada peça numa intrução que a máquina 
    teria de seguir.
    Fazedo o descrito anteriormente tinhamos o "mapa incial" todo trnasformado em simples instruções, cumprindo assim
    o primeiro objetivo proposto.
    De seguida é proposto que apliquemos um algoritomo de patter matching para que, seguindo os pressupostos pedidos, 
    reduzissemos ao máximo o tamanho da lista final de instruções. Para o fazer foi nos sugerido aplicar padrões.
    Introduzimos assim uma função que testa se existem Instruções iguais consecuticas, e se houverem, juntamos num novo
    tipo de instrução a "Repete", podendo então reduzir mais um bocado o número de intruções.
    
    = Discussão e Conclusão

    Esta tarefa requeria uma forma de pensamento diferente do que estamos habituados pois foi necessário repartir 
    pequenos problemas de patter matching e tantar de alguma maneira fazer com trabalhassem em conjunto.
    Os dois objectivos referidos em cima foram atingidos sem grande problema. 
    A função "transformaMapa" foi a função usada para transformar o mapa fornecido na lista de instruções pedida.
    A função "repete" foi a função usada para o pattern matching horizontal.
    Tentamos também acrescentar o padrão vertical, que funcionou para a maior parte dos casos e até nos deu melhor taxa
    de compressão no sistema de testes do  site da UC, mas em casos específicos dava erro, por isso, optamos pela 
    taxa de compressão um bocado pior mas com maior sucesso em termos de testes passados.
-}

module Tarefa3_2019li1g101 where

import LI11920

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [ [[Recta Terra 0,Rampa Terra 0 1,Recta Terra 1],[Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Rampa Terra 0 1,Rampa Relva 1 0]],[[Recta Terra 0,Rampa Relva 0 3,Recta Boost 3],[Recta Terra 0,Rampa Boost 0 4,Rampa Cola 4 8]],[[Recta Terra 0,Recta Terra 0, Recta Terra 0, Rampa Relva 0 1, Recta Lama 1, Recta Lama 1, Rampa Relva 1 0,Rampa Terra 0 1, Rampa Terra 1 2,Rampa Relva 2 3, Recta Cola 3,Recta Cola 3],[Recta Terra 0,Recta Terra 0, Rampa Relva 0 2, Rampa Relva 2 0, Recta Terra 0, Recta Terra 0,Rampa Boost 0 1, Recta Boost 1, Recta Boost 1, Rampa Boost 1 0,  Recta Terra 0, Recta Terra 0]] ]

--[[Recta Terra 0,Recta Terra 0, Recta Terra 0, Rampa Relva 0 1, Recta Lama 1, Recta Lama 1, Rampa Relva 1 0,Rampa Terra 0 1, Rampa Terra 1 2,Rampa Relva 2 3, Recta Cola 3,Recta Cola 3],
-- [Recta Terra 0,Recta Terra 0, Rampa Relva 0 2, Rampa Relva 2 0, Recta Terra 0, Recta Terra 0,Rampa Boost 0 1, Recta Boost 1, Recta Boost 1, Rampa Boost 1 0,  Recta Terra 0, Recta Terra 0]]

-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.

-- | Função que divide o mapa em instruções

desconstroi :: Mapa -- ^ Representa um mapa qualquer
               -> Instrucoes -- ^ Representa as instruções a dar ao(s) bulldozer(es)

desconstroi m = (repete (transformaMapa 0 (cutPeca m)))

-- | Função que resulta num mapa com as alterações efetuadas no piso

transformaMapa :: Int -- ^ Representa a posição da peça 
                  -> Mapa -- ^ Representa o mapa a transformar
                  -> Instrucoes -- ^ Representa as instruções a dar ao(s) bulldozer(es)

transformaMapa i []    = []
transformaMapa i (h:t) = (transformaPista i h) ++ (transformaMapa (i+1) t)

-- | Função que dá o piso a implementar 

transformaPista :: Int -- ^ Representa a posição da peça
                   -> Pista -- ^ Representa a pista a criar
                   -> Instrucoes -- ^ Representa as instruções a dar ao(s) bulldozer(es)

transformaPista i [] = []
transformaPista i ((Recta p a):t)     = (Anda [i] p):(transformaPista i t)
transformaPista i ((Rampa p a1 a2):t) = if (a1<a2) then Sobe  [i] p (a2-a1) : transformaPista i t
                                                   else Desce [i] p (a1-a2) : transformaPista i t

-- | Função que retira a primeira peça da pista

cutPeca :: Mapa -- ^ Representa um mapa qualquer
           -> Mapa -- ^ Representa um mapa sem a primeira peça

cutPeca [] = []
cutPeca t  = map tail t 

-- | Função que repete o número de instruções 

repete :: Instrucoes -- ^ Representa as instruções a dar ao(s) bulldozer(es)
          -> Instrucoes -- ^ Representa a compressão das intruções a dar ao(s) bulldozer(es)

repete [] = []
repete l | (again 1 l) == 1 = head l : repete (tail l)
         | (again 1 l) >  1 = (Repete (again 1 l) [(head l)]) : repete (drop (again 1 l) l)

-- | Função que conta o número de instruções iguais

again :: Int -- ^ Representa o contador
         -> Instrucoes -- ^ Representa as instruções a dar ao(s) bulldozer(es)
         -> Int -- ^ Representa o número de instruções iguais

again x [a] = x
again x (a:b:c) | a == b    = again (x+1) (b:c)
                | otherwise = x

