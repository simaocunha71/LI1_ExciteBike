{- |
    = Introdução
    Na tarefa 2, tivemos o objetivo de criar as jogadas que os jogadores podem fazer: 
    movimentarem-se para todas as direções, acelerarem, desacelerarem e dispararem 
    munições de cola para a peça anterior  
    
    = Objetivos
    
    O nosso principal objetivo era colocar as alterações do estado dos jogadores consoante 
    as jogadas efetuadas. Para tal, tivemos que criar varias funçoes auxiliares para cada 
    tipo de jogada acresentando a informação que está no enunciado, consoante se o jogador 
    está no ar, no chão ou no ar.

    = Discussão e conclusão

    Quando o jogador se movimenta, temos que ter cuidado com a altura das peças imediatamente 
    em cima ou em baixo e isso vai variar o seu estado (se vai estar morto, no ar ou no teclado)


-}

module Tarefa2_2019li1g101 where

import LI11920
import Data.Ratio
-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
     

testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0, Movimenta C, b), (0, Movimenta C, c), (0, Movimenta C, d), (0, Movimenta C, e), (0, Movimenta C, f), (0, Movimenta C, g), (0, Movimenta C, h), (0, Movimenta C, i),
            (1, Movimenta C, b), (1, Movimenta C, c), (1, Movimenta C, d), (1, Movimenta C, e), (1, Movimenta C, f), (1, Movimenta C, g), (1, Movimenta C, h), (1, Movimenta C, i),
            (0, Movimenta B, b), (0, Movimenta B, c), (0, Movimenta B, d), (0, Movimenta B, e), (0, Movimenta B, f), (0, Movimenta B, g), (0, Movimenta B, h), (0, Movimenta B, i),
            (1, Movimenta B, b), (1, Movimenta B, c1), (1, Movimenta B, d), (1, Movimenta B, e), (1, Movimenta B, f), (1, Movimenta B, g), (1, Movimenta B, h), (1, Movimenta B, i), 
            (1, Movimenta B, px), (0, Movimenta B, tx), (0, Movimenta B, tx2), 
            (0, Movimenta D, b), (0, Movimenta D, c), (0, Movimenta D, d), (0, Movimenta D, e), (0, Movimenta D, f), (0, Movimenta D, g), (0, Movimenta D, h), (0, Movimenta D, i),
            (1, Movimenta D, b), (1, Movimenta D, c), (1, Movimenta D, d), (1, Movimenta D, e), (1, Movimenta D, f), (1, Movimenta D, g), (1, Movimenta D, h), (1, Movimenta D, i),
            (0, Movimenta E, b), (0, Movimenta E, c), (0, Movimenta E, d), (0, Movimenta E, e), (0, Movimenta E, f), (0, Movimenta E, g), (0, Movimenta E, h), (0, Movimenta E, i),
            (1, Movimenta E, b), (1, Movimenta E, c), (1, Movimenta E, d), (1, Movimenta E, e), (1, Movimenta E, f), (1, Movimenta E, g), (1, Movimenta E, h), (1, Movimenta E, i),
            (0, Acelera, b), (0, Acelera, c), (0, Acelera, d), (0, Acelera, e), (0, Acelera, f), (0, Acelera, g), (0, Acelera, h), (0, Acelera, i),
            (1, Acelera, b), (1, Acelera, c), (1, Acelera, d), (1, Acelera, e), (1, Acelera, f), (1, Acelera, g), (1, Acelera, h), (1, Acelera, i),
            (0, Desacelera, b), (0, Desacelera, c), (0, Desacelera, d), (0, Desacelera, e), (0, Desacelera, f), (0, Desacelera, g), (0, Desacelera, i),
            (1, Desacelera, b), (1, Desacelera, c), (1, Desacelera, d), (1, Desacelera, e), (1, Desacelera, f), (1, Desacelera, g), (1, Desacelera, i),
            (0, Dispara, b), (0, Dispara, c), (0, Dispara, d), (0, Dispara, e), (0, Dispara, f), (0, Dispara, g), (0, Dispara, h), (0, Dispara, i),
            (1, Dispara, b), (1, Dispara, c), (1, Dispara, d), (1, Dispara, e), (1, Dispara, f), (1, Dispara, g), (1, Dispara, h), (1, Dispara, i),

            (0, Movimenta C, aa), (0, Movimenta C, ab), (0, Movimenta C, ac), (0, Movimenta C, ad), (0, Movimenta C, ae), (0, Movimenta C, af), (0, Movimenta B, ag), (0, Movimenta C, ah), (0, Movimenta C, ai), (0, Movimenta C, aj), (0, Movimenta C, ak), (0, Movimenta C, al),
            (1, Movimenta C, aa), (1, Movimenta C, ab), (1, Movimenta C, ac), (1, Movimenta C, ad), (1, Movimenta C, ae), (1, Movimenta C, af), (1, Movimenta B, ag), (1, Movimenta C, ah), (1, Movimenta C, ai), (1, Movimenta C, aj), (1, Movimenta C, ak), (1, Movimenta C, al),
            (2, Movimenta C, aa), (2, Movimenta C, ab), (2, Movimenta C, ac), (2, Movimenta C, ad), (2, Movimenta C, ae), (2, Movimenta C, af), (2, Movimenta B, ag), (2, Movimenta C, ah), (2, Movimenta C, ai), (2, Movimenta C, aj), (2, Movimenta C, ak), (2, Movimenta C, al),
            (3, Movimenta C, aa), (3, Movimenta C, ab), (3, Movimenta C, ac), (3, Movimenta C, ad), (3, Movimenta C, ae), (3, Movimenta C, af), (3, Movimenta B, ag), (3, Movimenta C, ah), (3, Movimenta C, ai), (3, Movimenta C, aj), (3, Movimenta C, ak), (3, Movimenta C, al),
            (4, Movimenta C, aa), (4, Movimenta C, ab), (4, Movimenta C, ac), (4, Movimenta C, ad), (4, Movimenta C, ae), (4, Movimenta C, af), (4, Movimenta B, ag), (4, Movimenta C, ah), (4, Movimenta C, ai), (4, Movimenta C, aj), (4, Movimenta C, ak), (4, Movimenta C, al),
            (5, Movimenta C, aa), (5, Movimenta C, ab), (5, Movimenta C, ac), (5, Movimenta C, ad), (5, Movimenta C, ae), (5, Movimenta C, af), (5, Movimenta B, ag), (5, Movimenta C, ah), (5, Movimenta C, ai), (5, Movimenta C, aj), (5, Movimenta C, ak), (5, Movimenta C, al),

            (0, Movimenta B, aa), (0, Movimenta B, ab), (0, Movimenta B, ac), (0, Movimenta B, ad), (0, Movimenta B, ae), (0, Movimenta B, af), (0, Movimenta B, ag), (0, Movimenta B, ah), (0, Movimenta B, ai), (0, Movimenta B, aj), (0, Movimenta B, ak), (0, Movimenta B, al),
            (1, Movimenta B, aa), (1, Movimenta B, ab), (1, Movimenta B, ac), (1, Movimenta B, ad), (1, Movimenta B, ae), (1, Movimenta B, af), (1, Movimenta B, ag), (1, Movimenta B, ah), (1, Movimenta B, ai), (1, Movimenta B, aj), (1, Movimenta B, ak), (1, Movimenta B, al),
            (2, Movimenta B, aa), (2, Movimenta B, ab), (2, Movimenta B, ac), (2, Movimenta B, ad), (2, Movimenta B, ae), (2, Movimenta B, af), (2, Movimenta B, ag), (2, Movimenta B, ah), (2, Movimenta B, ai), (2, Movimenta B, aj), (2, Movimenta B, ak), (2, Movimenta B, al),
            (3, Movimenta B, aa), (3, Movimenta B, ab), (3, Movimenta B, ac), (3, Movimenta B, ad), (3, Movimenta B, ae), (3, Movimenta B, af), (3, Movimenta B, ag), (3, Movimenta B, ah), (3, Movimenta B, ai), (3, Movimenta B, aj), (3, Movimenta B, ak), (3, Movimenta B, al),
            (4, Movimenta B, aa), (4, Movimenta B, ab), (4, Movimenta B, ac), (4, Movimenta B, ad), (4, Movimenta B, ae), (4, Movimenta B, af), (4, Movimenta B, ag), (4, Movimenta B, ah), (4, Movimenta B, ai), (4, Movimenta B, aj), (4, Movimenta B, ak), (4, Movimenta B, al),
            (5, Movimenta B, aa), (5, Movimenta B, ab), (5, Movimenta B, ac), (5, Movimenta B, ad), (5, Movimenta B, ae), (5, Movimenta B, af), (5, Movimenta B, ag), (5, Movimenta B, ah), (5, Movimenta B, ai), (5, Movimenta B, aj), (5, Movimenta B, ak), (5, Movimenta B, al),

            (0, Movimenta E, aa), (0, Movimenta E, ab), (0, Movimenta E, ac), (0, Movimenta E, ad), (0, Movimenta E, ae), (0, Movimenta E, af), (0, Movimenta E, ag), (0, Movimenta E, ah), (0, Movimenta E, ai), (0, Movimenta E, aj), (0, Movimenta E, ak), (0, Movimenta E, al),
            (1, Movimenta E, aa), (1, Movimenta E, ab), (1, Movimenta E, ac), (1, Movimenta E, ad), (1, Movimenta E, ae), (1, Movimenta E, af), (1, Movimenta E, ag), (1, Movimenta E, ah), (1, Movimenta E, ai), (1, Movimenta E, aj), (1, Movimenta E, ak), (1, Movimenta E, al),
            (2, Movimenta E, aa), (2, Movimenta E, ab), (2, Movimenta E, ac), (2, Movimenta E, ad), (2, Movimenta E, ae), (2, Movimenta E, af), (2, Movimenta E, ag), (2, Movimenta E, ah), (2, Movimenta E, ai), (2, Movimenta E, aj), (2, Movimenta E, ak), (2, Movimenta E, al),
            (3, Movimenta E, aa), (3, Movimenta E, ab), (3, Movimenta E, ac), (3, Movimenta E, ad), (3, Movimenta E, ae), (3, Movimenta E, af), (3, Movimenta E, ag), (3, Movimenta E, ah), (3, Movimenta E, ai), (3, Movimenta E, aj), (3, Movimenta E, ak), (3, Movimenta E, al),
            (4, Movimenta E, aa), (4, Movimenta E, ab), (4, Movimenta E, ac), (4, Movimenta E, ad), (4, Movimenta E, ae), (4, Movimenta E, af), (4, Movimenta E, ag), (4, Movimenta E, ah), (4, Movimenta E, ai), (4, Movimenta E, aj), (4, Movimenta E, ak), (4, Movimenta E, al),
            (5, Movimenta E, aa), (5, Movimenta E, ab), (5, Movimenta E, ac), (5, Movimenta E, ad), (5, Movimenta E, ae), (5, Movimenta E, af), (5, Movimenta E, ag), (5, Movimenta E, ah), (5, Movimenta E, ai), (5, Movimenta E, aj), (5, Movimenta E, ak), (5, Movimenta E, al),

            (0, Movimenta D, aa), (0, Movimenta D, ab), (0, Movimenta D, ac), (0, Movimenta D, ad), (0, Movimenta D, ae), (0, Movimenta D, af), (0, Movimenta D, ag), (0, Movimenta D, ah), (0, Movimenta D, ai), (0, Movimenta D, aj), (0, Movimenta D, ak), (0, Movimenta D, al),
            (1, Movimenta D, aa), (1, Movimenta D, ab), (1, Movimenta D, ac), (1, Movimenta D, ad), (1, Movimenta D, ae), (1, Movimenta D, af), (1, Movimenta D, ag), (1, Movimenta D, ah), (1, Movimenta D, ai), (1, Movimenta D, aj), (1, Movimenta D, ak), (1, Movimenta D, al),
            (2, Movimenta D, aa), (2, Movimenta D, ab), (2, Movimenta D, ac), (2, Movimenta D, ad), (2, Movimenta D, ae), (2, Movimenta D, af), (2, Movimenta D, ag), (2, Movimenta D, ah), (2, Movimenta D, ai), (2, Movimenta D, aj), (2, Movimenta D, ak), (2, Movimenta D, al),
            (3, Movimenta D, aa), (3, Movimenta D, ab), (3, Movimenta D, ac), (3, Movimenta D, ad), (3, Movimenta D, ae), (3, Movimenta D, af), (3, Movimenta D, ag), (3, Movimenta D, ah), (3, Movimenta D, ai), (3, Movimenta D, aj), (3, Movimenta D, ak), (3, Movimenta D, al),
            (4, Movimenta D, aa), (4, Movimenta D, ab), (4, Movimenta D, ac), (4, Movimenta D, ad), (4, Movimenta D, ae), (4, Movimenta D, af), (4, Movimenta D, ag), (4, Movimenta D, ah), (4, Movimenta D, ai), (4, Movimenta D, aj), (4, Movimenta D, ak), (4, Movimenta D, al),
            (5, Movimenta D, aa), (5, Movimenta D, ab), (5, Movimenta D, ac), (5, Movimenta D, ad), (5, Movimenta D, ae), (5, Movimenta D, af), (5, Movimenta D, ag), (5, Movimenta D, ah), (5, Movimenta D, ai), (5, Movimenta D, aj), (5, Movimenta D, ak), (5, Movimenta D, al),

            (0, Acelera, aa), (0, Acelera, ab), (0, Acelera, ac), (0, Acelera, ad), (0, Acelera, ae), (0, Acelera, af), (0, Acelera, ag), (0, Acelera, ah), (0, Acelera, ai), (0, Acelera, aj), (0, Acelera, ak), (0, Acelera, al),
            (1, Acelera, aa), (1, Acelera, ab), (1, Acelera, ac), (1, Acelera, ad), (1, Acelera, ae), (1, Acelera, af), (1, Acelera, ag), (1, Acelera, ah), (1, Acelera, ai), (1, Acelera, aj), (1, Acelera, ak), (1, Acelera, al),
            (2, Acelera, aa), (2, Acelera, ab), (2, Acelera, ac), (2, Acelera, ad), (2, Acelera, ae), (2, Acelera, af), (2, Acelera, ag), (2, Acelera, ah), (2, Acelera, ai), (2, Acelera, aj), (2, Acelera, ak), (2, Acelera, al),
            (3, Acelera, aa), (3, Acelera, ab), (3, Acelera, ac), (3, Acelera, ad), (3, Acelera, ae), (3, Acelera, af), (3, Acelera, ag), (3, Acelera, ah), (3, Acelera, ai), (3, Acelera, aj), (3, Acelera, ak), (3, Acelera, al),
            (4, Acelera, aa), (4, Acelera, ab), (4, Acelera, ac), (4, Acelera, ad), (4, Acelera, ae), (4, Acelera, af), (4, Acelera, ag), (4, Acelera, ah), (4, Acelera, ai), (4, Acelera, aj), (4, Acelera, ak), (4, Acelera, al),
            (5, Acelera, aa), (5, Acelera, ab), (5, Acelera, ac), (5, Acelera, ad), (5, Acelera, ae), (5, Acelera, af), (5, Acelera, ag), (5, Acelera, ah), (5, Acelera, ai), (5, Acelera, aj), (5, Acelera, ak), (5, Acelera, al),

            (0, Desacelera, aa), (0, Desacelera, ab), (0, Desacelera, ac), (0, Desacelera, ad), (0, Desacelera, ae), (0, Desacelera, af), (0, Desacelera, ag), (0, Desacelera, ah), (0, Desacelera, ai), (0, Desacelera, aj), (0, Desacelera, ak), (0, Desacelera, al),
            (1, Desacelera, aa), (1, Desacelera, ab), (1, Desacelera, ac), (1, Desacelera, ad), (1, Desacelera, ae), (1, Desacelera, af), (1, Desacelera, ag), (1, Desacelera, ah), (1, Desacelera, ai), (1, Desacelera, aj), (1, Desacelera, ak), (1, Desacelera, al),
            (2, Desacelera, aa), (2, Desacelera, ab), (2, Desacelera, ac), (2, Desacelera, ad), (2, Desacelera, ae), (2, Desacelera, af), (2, Desacelera, ag), (2, Desacelera, ah), (2, Desacelera, ai), (2, Desacelera, aj), (2, Desacelera, ak), (2, Desacelera, al),
            (3, Desacelera, aa), (3, Desacelera, ab), (3, Desacelera, ac), (3, Desacelera, ad), (3, Desacelera, ae), (3, Desacelera, af), (3, Desacelera, ag), (3, Desacelera, ah), (3, Desacelera, ai), (3, Desacelera, aj), (3, Desacelera, ak), (3, Desacelera, al),
            (4, Desacelera, aa), (4, Desacelera, ab), (4, Desacelera, ac), (4, Desacelera, ad), (4, Desacelera, ae), (4, Desacelera, af), (4, Desacelera, ag), (4, Desacelera, ah), (4, Desacelera, ai), (4, Desacelera, aj), (4, Desacelera, ak), (4, Desacelera, al),
            (5, Desacelera, aa), (5, Desacelera, ab), (5, Desacelera, ac), (5, Desacelera, ad), (5, Desacelera, ae), (5, Desacelera, af), (5, Desacelera, ag), (5, Desacelera, ah), (5, Desacelera, ai), (5, Desacelera, aj), (5, Desacelera, ak), (5, Desacelera, al),

            (0, Dispara, aa), (0, Dispara, ab), (0, Dispara, ac), (0, Dispara, ad), (0, Dispara, ae), (0, Dispara, af), (0, Dispara, ag), (0, Dispara, ah), (0, Dispara, ai), (0, Dispara, aj), (0, Dispara, ak), (0, Dispara, al),
            (1, Dispara, aa), (1, Dispara, ab), (1, Dispara, ac), (1, Dispara, ad), (1, Dispara, ae), (1, Dispara, af), (1, Dispara, ag), (1, Dispara, ah), (1, Dispara, ai), (1, Dispara, aj), (1, Dispara, ak), (1, Dispara, al),
            (2, Dispara, aa), (2, Dispara, ab), (2, Dispara, ac), (2, Dispara, ad), (2, Dispara, ae), (2, Dispara, af), (2, Dispara, ag), (2, Dispara, ah), (2, Dispara, ai), (2, Dispara, aj), (2, Dispara, ak), (2, Dispara, al),
            (3, Dispara, aa), (3, Dispara, ab), (3, Dispara, ac), (3, Dispara, ad), (3, Dispara, ae), (3, Dispara, af), (3, Dispara, ag), (3, Dispara, ah), (3, Dispara, ai), (3, Dispara, aj), (3, Dispara, ak), (3, Dispara, al),
            (4, Dispara, aa), (4, Dispara, ab), (4, Dispara, ac), (4, Dispara, ad), (4, Dispara, ae), (4, Dispara, af), (4, Dispara, ag), (4, Dispara, ah), (4, Dispara, ai), (4, Dispara, aj), (4, Dispara, ak), (4, Dispara, al),
            (5, Dispara, aa), (5, Dispara, ab), (5, Dispara, ac), (5, Dispara, ad), (5, Dispara, ae), (5, Dispara, af), (5, Dispara, ag), (5, Dispara, ah), (5, Dispara, ai), (5, Dispara, aj), (5, Dispara, ak), (5, Dispara, al),

            (0, Movimenta B, rectaRecta), (1, Movimenta C, rectaRecta), (0, Movimenta B, rectaRampa), (1, Movimenta C, rectaRampa), (0, Movimenta B, rampaRampa), (1, Movimenta C, rampaRampa),
            (0, Movimenta B, rectaRecta4), (1, Movimenta C, rectaRecta4), (0, Movimenta B, rectaRampa4), (1, Movimenta C, rectaRampa4), (0, Movimenta B, rampaRampa4), (1, Movimenta C, rampaRampa4),
            (0, Movimenta B, rampaRecta_2), (1, Movimenta C, rampaRecta_2), (0, Movimenta B, testeQualquer), (1, Movimenta C, testeQualquer), (0, Movimenta B, testeQualquer_2), (1, Movimenta C, testeQualquer_2)]

            where 
            
            b = (Estado mapaTeste1 listaJogadoresTeste2)
            c = (Estado mapaTeste1 listaJogadoresTeste3)
            c1 = (Estado mapaTeste1 listaJogadoresTeste3_my)
            d = (Estado mapaTeste1 listaJogadoresTeste4)
            e = (Estado mapaTeste1 listaJogadoresTeste5)
            f = (Estado mapaTeste1 listaJogadoresTeste6)
            g = (Estado mapaTeste1 listaJogadoresTeste7)
            h = (Estado mapaTeste1 listaJogadoresTeste8)
            i = (Estado mapaTeste1 listaJogadoresTeste9)
            px = (Estado mapaTeste1 listaJogadoresTeste10)
            tx = (Estado mapaTeste1 listaJogadoresTeste11)
            tx2 = (Estado mapaTeste1 listaJogadoresTeste12_my)
            ----------------------------------------------
            aa = (Estado mapaTeste2 listaJogadoresTesteAA)
            ab = (Estado mapaTeste2 listaJogadoresTeste3AB)
            ac = (Estado mapaTeste2 listaJogadoresTeste3_myAC)
            ad = (Estado mapaTeste2 listaJogadoresTeste9AD)
            ae = (Estado mapaTeste2 listaJogadoresTeste4AE)
            af = (Estado mapaTeste2 listaJogadoresTeste5AF)
            ag = (Estado mapaTeste2 listaJogadoresTeste6AG)
            ah = (Estado mapaTeste2 listaJogadoresTeste7AH)
            ai = (Estado mapaTeste2 listaJogadoresTeste8AI)
            aj = (Estado mapaTeste2 listaJogadoresTeste10AJ)
            ak = (Estado mapaTeste2 listaJogadoresTeste11AK)
            al = (Estado mapaTeste2 listaJogadoresTeste12AL)
            rectaRecta = (Estado mapaTeste3 listaJogadoresTestePeca3)
            rectaRampa = (Estado mapaTeste3 listaJogadoresTestePeca2)
            rampaRampa = (Estado mapaTeste3 listaJogadoresTestePeca4)
            rectaRecta4 = (Estado mapaTeste4 listaJogadoresTestePeca3)
            rectaRampa4 = (Estado mapaTeste4 listaJogadoresTestePeca2)
            rampaRampa4 = (Estado mapaTeste4 listaJogadoresTestePeca4)
            rampaRecta_2 = (Estado mapaTeste5 listaJogadoresTestePeca5)
            testeQualquer = (Estado mapaTeste5 listaJogadoresTestePeca6)
            testeQualquer_2 = (Estado mapaTeste6 listaJogadoresTestePeca7)


mapaTeste1 :: Mapa 
mapaTeste1 = [[Recta Terra 0, Recta Boost 0, Rampa Relva 0 1, Rampa Relva 1 0], 
              [Recta Terra 0, Rampa Relva 0 1, Rampa Boost 1 0, Recta Relva 0], 
              [Recta Terra 0, Rampa Relva 0 2 ,Recta Terra 2, Recta Terra 2],
              [Recta Terra 0, Recta Terra 0, Rampa Relva 0 2, Rampa Lama 2 1]]    

mapaTeste2 :: Mapa 
mapaTeste2 = [[Recta Terra 0, Recta Lama 0, Rampa Lama 0 2, Rampa Lama 2 1, Recta Relva 1, Recta Lama 1],
              [Recta Terra 0, Recta Lama 0, Rampa Lama 0 2, Recta Lama 2, Rampa Boost 2 3, Rampa Boost 3 0],
              [Recta Terra 0, Recta Lama 0, Rampa Lama 0 1, Rampa Relva 1 0, Rampa Boost 0 2, Rampa Boost 2 0],
              [Recta Terra 0, Recta Lama 0, Rampa Lama 0 1, Recta Relva 1, Rampa Boost 1 0, Recta Boost 0],
              [Recta Terra 0, Recta Lama 0, Recta Relva 0, Rampa Boost 0 3, Rampa Terra 3 2, Rampa Boost 2 3],
              [Recta Terra 0, Recta Lama 0, Recta Relva 0, Rampa Boost 0 1, Rampa Terra 1 0, Recta Relva 0]]

mapaTeste3 :: Mapa
mapaTeste3 = [[Recta Terra 0, Recta Lama 0, Recta Terra 0, Rampa Boost 0 2],
              [Recta Terra 0, Rampa Lama 0 2, Recta Lama 2, Rampa Boost 2 0]]

mapaTeste4 :: Mapa
mapaTeste4 = [[Recta Terra 0, Recta Lama 0, Recta Terra 0, Rampa Boost 0 6],
              [Recta Terra 0, Rampa Lama 0 7, Recta Lama 7, Rampa Boost 7 0]]             

mapaTeste5 :: Mapa
mapaTeste5 = [[Recta Terra 0, Rampa Relva 0 1, Recta Terra 1, Rampa Boost 1 3],
              [Recta Terra 0, Rampa Lama 0 1, Recta Lama 1, Rampa Boost 1 0]]

mapaTeste6 :: Mapa
mapaTeste6 = [[Recta Terra 0, Rampa Relva 0 1, Recta Terra 1, Rampa Boost 1 3],
              [Recta Terra 0, Recta Boost 0, Recta Lama 0, Recta Relva 0]]              

listaJogadoresTestePeca3 :: [Jogador] 
listaJogadoresTestePeca3 = [Jogador 0 2.5 0 5 (Chao True), Jogador 1 3 0 5 (Chao True)]

listaJogadoresTestePeca2 :: [Jogador] 
listaJogadoresTestePeca2 = [Jogador 0 1.5 0 5 (Chao True), Jogador 1 2 0 5 (Chao True)]

listaJogadoresTestePeca4 :: [Jogador] 
listaJogadoresTestePeca4 = [Jogador 0 3.5 0 5 (Chao True), Jogador 1 3.8 0 5 (Chao True)]

listaJogadoresTestePeca5 :: [Jogador] 
listaJogadoresTestePeca5 = [Jogador 0 1.1 0 5 (Chao True), Jogador 1 1.6 0 5 (Chao True)]

listaJogadoresTestePeca6 :: [Jogador] 
listaJogadoresTestePeca6 = [Jogador 0 0.5 0 5 (Ar 1.0 30.1 9.8), Jogador 1 0.5 0 5 (Ar 1.0 30.1 9.8)]

listaJogadoresTestePeca7 :: [Jogador] 
listaJogadoresTestePeca7 = [Jogador 0 0.1 0 5 (Chao True), Jogador 1 0.1 0 5 (Chao True)]

listaJogadoresTeste2 :: [Jogador] 
listaJogadoresTeste2 = [Jogador 0 0 0 5 (Chao True), Jogador 0 1 1 0 (Chao False)]

listaJogadoresTeste3 :: [Jogador]
listaJogadoresTeste3 = [Jogador 0 0.5 0.5 1 (Chao True), Jogador 1 1.9 1.9 2 (Chao False)]

listaJogadoresTeste3_my :: [Jogador]
listaJogadoresTeste3_my = [Jogador 0 0.5 0.5 1 (Chao True), Jogador 1 1.9 1.9 2 (Ar 1.0 30.1 9.8)]

listaJogadoresTeste9 :: [Jogador]
listaJogadoresTeste9 = [Jogador 0 0 0 5 (Morto 1.0), Jogador 0 1 1 0 (Morto 1.0)]

listaJogadoresTeste4 :: [Jogador]
listaJogadoresTeste4 = [Jogador 0 0.5 0.5 1 (Morto 1.0), Jogador 1 1.9 1.9 2 (Morto 1.0)]

listaJogadoresTeste5 :: [Jogador]
listaJogadoresTeste5 = [Jogador 0 0 0 5 (Ar 0 0 9.8), Jogador 0 1 1 0 (Ar 1 90.0 9.8)]

listaJogadoresTeste6 :: [Jogador]
listaJogadoresTeste6 = [Jogador 0 0.5 0.5 1 (Ar 0.6 (-90.0) 9.8), Jogador 1 1.9 1.9 2 (Ar 1.9 (-40.5) 9.8)]

listaJogadoresTeste7 :: [Jogador]
listaJogadoresTeste7 = [Jogador 0 0.5 0.5 1 (Ar 0.6 15.0 9.8), Jogador 1 1.9 1.9 2 (Ar 1.9 40.5 9.8)]

listaJogadoresTeste8 :: [Jogador]
listaJogadoresTeste8 = [Jogador 0 0.5 0.5 1 (Ar 0.6 30.1 9.8), Jogador 1 1.9 1.9 2 (Ar 1.9 (-15.0) 9.8)]

listaJogadoresTeste10 :: [Jogador]
listaJogadoresTeste10 = [Jogador 3 0 0 5 (Chao True), Jogador 0 1 1 0 (Chao False)]

listaJogadoresTeste11 :: [Jogador]
listaJogadoresTeste11 = [Jogador 1 3.5 0 5 (Chao True), Jogador 0 1 1 0 (Chao False)]

listaJogadoresTeste12_my :: [Jogador]
listaJogadoresTeste12_my = [Jogador 1 3.5 0 5 (Morto 1.0), Jogador 0 1 1 0 (Chao False)]
-------------------------------------------------------------------------------------------------------------------------
listaJogadoresTesteAA :: [Jogador] 
listaJogadoresTesteAA = [Jogador 0 0 0 5 (Chao True), Jogador 1 1 1 0 (Chao False), Jogador 2 0 0 5 (Chao True), Jogador 3 1 1 0 (Chao False), Jogador 4 0 0 5 (Chao True), Jogador 5 1 1 0 (Chao False)]

listaJogadoresTeste3AB :: [Jogador]
listaJogadoresTeste3AB = [Jogador 0 0.5 0.5 1 (Chao True), Jogador 1 1.9 1.9 2 (Chao False), Jogador 2 0.5 0.5 1 (Chao True), Jogador 3 1.9 1.9 2 (Chao False), Jogador 4 0.5 0.5 1 (Chao True), Jogador 5 1.9 1.9 2 (Chao False)]

listaJogadoresTeste3_myAC :: [Jogador]
listaJogadoresTeste3_myAC = [Jogador 0 0.5 0.5 1 (Chao True), Jogador 1 1.9 1.9 2 (Ar 1.0 30.1 9.8), Jogador 2 0.5 0.5 1 (Chao True), Jogador 3 1.9 1.9 2 (Ar 1.0 30.1 9.8), Jogador 4 0.5 0.5 1 (Chao True), Jogador 5 1.9 1.9 2 (Ar 1.0 30.1 9.8)]

listaJogadoresTeste9AD :: [Jogador]
listaJogadoresTeste9AD = [Jogador 0 0 0 5 (Morto 1.0), Jogador 1 1 1 0 (Morto 1.0), Jogador 2 0 0 5 (Morto 1.0), Jogador 3 1 1 0 (Morto 1.0), Jogador 4 0 0 5 (Morto 1.0), Jogador 5 1 1 0 (Morto 1.0)]

listaJogadoresTeste4AE :: [Jogador]
listaJogadoresTeste4AE = [Jogador 0 0.5 0.5 1 (Morto 1.0), Jogador 1 1.9 1.9 2 (Morto 1.0), Jogador 2 0.5 0.5 1 (Morto 1.0), Jogador 3 1.9 1.9 2 (Morto 1.0), Jogador 4 0.5 0.5 1 (Morto 1.0), Jogador 5 1.9 1.9 2 (Morto 1.0)]

listaJogadoresTeste5AF :: [Jogador]
listaJogadoresTeste5AF = [Jogador 0 0 0 5 (Ar 0 0 9.8), Jogador 1 1 1 0 (Ar 1 90.0 9.8), Jogador 2 0 0 5 (Ar 0 0 9.8), Jogador 3 1 1 0 (Ar 1 90.0 9.8), Jogador 4 0 0 5 (Ar 0 0 9.8), Jogador 5 1 1 0 (Ar 1 90.0 9.8)]

listaJogadoresTeste6AG :: [Jogador]
listaJogadoresTeste6AG = [Jogador 0 0.5 0.5 1 (Ar 0.6 (-90.0) 9.8), Jogador 1 1.9 1.9 2 (Ar 1.9 (-40.5) 9.8), Jogador 2 0.5 0.5 1 (Ar 0.6 (-90.0) 9.8), Jogador 3 1.9 1.9 2 (Ar 1.9 (-40.5) 9.8), Jogador 4 0.5 0.5 1 (Ar 0.6 (-90.0) 9.8), Jogador 5 1.9 1.9 2 (Ar 1.9 (-40.5) 9.8)]

listaJogadoresTeste7AH :: [Jogador]
listaJogadoresTeste7AH = [Jogador 0 0.5 0.5 1 (Ar 0.6 15.0 9.8), Jogador 1 1.9 1.9 2 (Ar 1.9 40.5 9.8), Jogador 2 0.5 0.5 1 (Ar 0.6 15.0 9.8), Jogador 3 1.9 1.9 2 (Ar 1.9 40.5 9.8), Jogador 4 0.5 0.5 1 (Ar 0.6 15.0 9.8), Jogador 5 1.9 1.9 2 (Ar 1.9 40.5 9.8)]

listaJogadoresTeste8AI :: [Jogador]
listaJogadoresTeste8AI = [Jogador 0 0.5 0.5 1 (Ar 0.6 30.1 9.8), Jogador 1 1.9 1.9 2 (Ar 1.9 (-15.0) 9.8), Jogador 2 0.5 0.5 1 (Ar 0.6 30.1 9.8), Jogador 3 1.9 1.9 2 (Ar 1.9 (-15.0) 9.8), Jogador 4 0.5 0.5 1 (Ar 0.6 30.1 9.8), Jogador 5 1.9 1.9 2 (Ar 1.9 (-15.0) 9.8)]

listaJogadoresTeste10AJ :: [Jogador]
listaJogadoresTeste10AJ = [Jogador 3 0 0 5 (Chao True), Jogador 0 1 1 0 (Chao False), Jogador 1 0 0 5 (Chao True), Jogador 2 1 1 0 (Chao False), Jogador 4 0 0 5 (Chao True), Jogador 5 1 1 0 (Chao False)]

listaJogadoresTeste11AK :: [Jogador]
listaJogadoresTeste11AK = [Jogador 2 3.5 0 5 (Chao True), Jogador 0 1 1 0 (Chao False), Jogador 1 3.5 0 5 (Chao True), Jogador 3 1 1 0 (Chao False), Jogador 4 3.5 0 5 (Chao True), Jogador 5 1 1 0 (Chao False)]

listaJogadoresTeste12AL :: [Jogador]
listaJogadoresTeste12AL = [Jogador 1 3.5 0 5 (Chao True), Jogador 0 1 1 0 (Chao False), Jogador 2 3.5 0 5 (Chao True), Jogador 3 1 1 0 (Chao False), Jogador 4 3.5 0 5 (Chao True), Jogador 5 1 1 0 (Chao False)]                                   
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.

-- | O jogador pode movimentar nas 4 direções (Cma, Baixo, Esquerda e Direita), Acelerar, Desacelerar e Disparar. Seguem-se as funções que permitem ao jogador efetuar tal ato

jogada n (Movimenta d)  e | (estaMorto((jogadoresEstado e) !! n)) || 
                            (d == E && estaNoChao((jogadoresEstado e) !! n)) ||
                            (d == D && estaNoChao((jogadoresEstado e) !! n)) ||
                            (d == C && (estaNoChao((jogadoresEstado e) !! n)) == False) ||
                            (d == B && (estaNoChao((jogadoresEstado e) !! n)) == False)
                          = Estado {mapaEstado = mapaEstado e, jogadoresEstado = jogadoresEstado e}
                          | otherwise = funcMovimenta n d e

jogada n Acelera        e | estaMorto((jogadoresEstado e) !! n) ||
                            estaNoChao((jogadoresEstado e) !! n) == False     
                          = Estado {mapaEstado = mapaEstado e, jogadoresEstado = jogadoresEstado e} 
                          | otherwise = funcAcelera n e

jogada n Desacelera     e | estaMorto((jogadoresEstado e) !! n) ||
                            estaNoChao((jogadoresEstado e) !! n) == False      
                          = Estado {mapaEstado = mapaEstado e, jogadoresEstado = jogadoresEstado e}
                          | otherwise = funcDesacelera n e

jogada n Dispara        e | estaMorto((jogadoresEstado e) !! n) ||
                            estaNoChao((jogadoresEstado e) !! n) == False   
                          = Estado {mapaEstado = mapaEstado e, jogadoresEstado = jogadoresEstado e}
                          | otherwise = funcDispara n e 


----------------------------------------------------------
--funções auxiliares para Movimenta CIMA OU BAIXO
----------------------------------------------------------

-- | Função com argumentos uma dada direção e num estado anterior e retorna um estado com a nova direção efetuada

funcMovimenta :: Int -- ^ Serve como contador do jogador
                 -> Direcao -- ^ Representa a direção a efetuar
                 -> Estado -- ^ Representa o estado anterior
                 -> Estado -- ^ Representa o estado desejado

funcMovimenta n C e = if((pistaJogador((jogadoresEstado e) !! n)) <= 0) 
                      then Estado {mapaEstado = mapaEstado e, jogadoresEstado = jogadoresEstado e}
                      else Estado {mapaEstado = mapaEstado e, jogadoresEstado = mudaPistaCima (mapaEstado e) n C (jogadoresEstado e) } 

funcMovimenta n B e = if((pistaJogador((jogadoresEstado e)!! n)) >= (length(jogadoresEstado e)-1)) 
                      then Estado {mapaEstado = mapaEstado e, jogadoresEstado = jogadoresEstado e}
                      else Estado {mapaEstado = mapaEstado e, jogadoresEstado = mudaPistaBaixo (mapaEstado e) n B (jogadoresEstado e)}

funcMovimenta n D e = if(inclinacaoJogador(estadoJogador((jogadoresEstado e)!!n)) < (-75.00))
                      then Estado {mapaEstado = mapaEstado e, jogadoresEstado = mudaDirecaoFull n D (jogadoresEstado e)}
                      else Estado {mapaEstado = mapaEstado e, jogadoresEstado = mudaDirecao n D (jogadoresEstado e)} 

funcMovimenta n E e = if(inclinacaoJogador(estadoJogador((jogadoresEstado e)!!n)) > (75.00))
                      then Estado {mapaEstado = mapaEstado e, jogadoresEstado = mudaDirecaoFull n E (jogadoresEstado e)}
                      else Estado {mapaEstado = mapaEstado e, jogadoresEstado = mudaDirecao n E (jogadoresEstado e)} 

-- | Função que dá a lista de jogadores com a direção alterada

mudaDirecaoFull :: Int -- ^ Serve como contador do jogador
                   -> Direcao -- ^ Representa a direção "Esquerda" ou "Direita"
                   -> [Jogador] -- ^ Lista de jogadores a jogar
                   ->[Jogador] -- ^ Representa a lista de jogadores com a direção alterada

mudaDirecaoFull _ _ [] = []
mudaDirecaoFull n d (h:t) |n == 0 && d == D = (Jogador{pistaJogador      = pistaJogador h ,
                                                   distanciaJogador  = distanciaJogador h,
                                                   velocidadeJogador = velocidadeJogador h,
                                                   colaJogador       = colaJogador h,
                                                   estadoJogador     = mdirjogadorFull D (estadoJogador h)}): t
                      |n == 0 && d == E = (Jogador{pistaJogador      = pistaJogador h ,
                                                   distanciaJogador  = distanciaJogador h,
                                                   velocidadeJogador = velocidadeJogador h,
                                                   colaJogador       = colaJogador h,
                                                   estadoJogador     = mdirjogadorFull E (estadoJogador h)}): t
                      |otherwise = h : mudaDirecaoFull (n-1) d t

-- | Função que transforma o estado do jogador para "Ar"

mdirjogadorFull :: Direcao -- ^ Representa uma direção qualquer
                   -> EstadoJogador -- ^ Representa um estado de jogador qualquer
                   -> EstadoJogador -- ^ Representa o estado de jogador "Ar"

mdirjogadorFull d e = (Ar{alturaJogador     = alturaJogador e, 
                      inclinacaoJogador = mdirFull d,
                      gravidadeJogador  = gravidadeJogador e}) 

-- | Função que determina a inclinação máxima quando o jogador desloca-se para a direita ou para a esquerda

mdirFull :: Direcao -- ^ Representa uma direção qualquer
            -> Double -- ^ Representa a inclinação final (máxima) quando o a inclinação anterior é mair ou  igual a 75 no caso de move Esquerda ou menor que -75 quando é move Esquerda

mdirFull d | d == D = -90 
           | d == E = 90

-- | Função que altera a inclinação (aumenta ou diminui) conforme a direção Direira ou Esquerda

mdir :: Direcao -- ^ Representa uma direção qualquer
        -> Double -- ^ Representa a altura do Jogador em causa
        -> Double -- ^ Representa a inclinação

mdir d x | d == D = x + (-15)  
         | d == E = x + 15

-- | Função que muda o parâmetro inclinação (inclinacaoJogador), incluído no "EstadoJogador"

mdirjogador :: Direcao -- ^ Representa uma direção qualquer
               -> EstadoJogador -- ^ Representa um estado de jogador qualquer
               -> EstadoJogador -- ^ Representa um estado de jogador alterado

mdirjogador d e = (Ar{alturaJogador     = alturaJogador e, 
                      inclinacaoJogador = mdir d (inclinacaoJogador e),
                      gravidadeJogador  = gravidadeJogador e}) 
                              
-- | Função que muda o estado do jogador consoante a direção atribuída 

mudaDirecao :: Int -- ^ Serve como contador do jogador
               -> Direcao -- ^ Representa a direção "Esquerda" ou "Direita"
               -> [Jogador] -- ^ Lista de jogadores a jogar
               -> [Jogador] -- ^ Representa a lista de jogadores com a direção alterada

mudaDirecao n d [] = []
mudaDirecao n d (h:t) |n == 0 && d == D = (Jogador{pistaJogador      = pistaJogador h ,
                                                   distanciaJogador  = distanciaJogador h,
                                                   velocidadeJogador = velocidadeJogador h,
                                                   colaJogador       = colaJogador h,
                                                   estadoJogador     = mdirjogador D (estadoJogador h)}): t
                      |n == 0 && d == E = (Jogador{pistaJogador      = pistaJogador h ,
                                                   distanciaJogador  = distanciaJogador h,
                                                   velocidadeJogador = velocidadeJogador h,
                                                   colaJogador       = colaJogador h,
                                                   estadoJogador     = mdirjogador E (estadoJogador h)}): t
                      |otherwise = h : mudaDirecao (n-1) d t

-- | Função que determina a altura da peça

alturaPeca :: Peca -- ^ Representa a peça escolhida
              -> Double -- ^ Representa a parte decimal da distancia em que o jogador se encontra na Peca
              -> Double -- ^ Representa a altura da peça

alturaPeca (Recta p b) x   = fromIntegral (b)
alturaPeca (Rampa p a b) x | (x>0) = (fromIntegral(b-a) * x) + fromIntegral(min a b) 
                           | otherwise = fromIntegral(a) + fromIntegral(min a b) 

-- | Função que retorna um jogador, quando ao movimentar para Cima ou para Baixo a diferença das alturas das peças são <= 0.2 ou > 0.2. Aqui vamos ter várias variantes, com Recta/Recta, Recta/Rampa, Rampa/Recta e Rampa/Rampa

alteracaoJogador :: Jogador -- ^ Representa o jogador escolhido para fazer a alteração dos seus parâmetros
                    -> Peca -- ^ Representa a peça inicial
                    -> Peca -- ^ Representa a peça final
                    -> Double -- ^ Representa a parte decimal das alturas das peças
                    -> Jogador -- ^ Representa o jogador com as devidas alterações


alteracaoJogador (Jogador p d v c (Chao z1)) (Recta x a) (Recta x1 a1) t = if ((abs ((alturaPeca (Recta x a) t) - (alturaPeca(Recta x1 a1) t))) <= 0.2) then (Jogador p d v c (Chao z1)) 
                                                                             else if ((alturaPeca(Recta x1 a1) t) > (alturaPeca (Recta x a) t)) 
                                                                                  then (Jogador p d v c (Morto 1.0)) 
                                                                                  else (Jogador p d v c (Ar ((alturaPeca (Recta x a) t)) (toGraus (atan(((alturaPeca(Recta x1 a1) t))-((alturaPeca (Recta x a) t))))) 0))

alteracaoJogador (Jogador p d v c (Chao z1)) (Recta x a) (Rampa x1 a1 a2) t = if ((abs ((alturaPeca (Recta x a) t) - (alturaPeca(Rampa x1 a1 a2) t))) <= 0.2) then (Jogador p d v c (Chao z1))
                                                                                else if (alturaPeca(Rampa x1 a1 a2) t) > (alturaPeca (Recta x a) t)
                                                                                     then (Jogador p d v c (Morto 1.0)) 
                                                                                     else (Jogador p d v c (Ar (alturaPeca (Recta x a) t) (toGraus (atan(((alturaPeca(Rampa x1 a1 a2) t))-((alturaPeca (Recta x a) t))))) 0))                                                              

alteracaoJogador (Jogador p d v c (Chao z1)) (Rampa x1 a1 a2)(Recta x a) t = if ((abs ((alturaPeca (Rampa x1 a1 a2) t) - (alturaPeca (Recta x a) t))) <= 0.2) 
                                                                               then (Jogador p d v c (Chao z1)) 
                                                                               else if (alturaPeca (Recta x a) t) > (alturaPeca (Rampa x1 a1 a2) t)
                                                                                    then (Jogador p d v c (Morto 1.0)) 
                                                                                    else (Jogador p d v c (Ar (alturaPeca (Rampa x1 a1 a2)t) (toGraus (atan((fromIntegral a2)-(fromIntegral a1)))) 0))                                                             

alteracaoJogador (Jogador p d v c (Chao z1)) (Rampa x1 a1 a2)(Rampa x2 b1 b2) t = if (((abs ((alturaPeca (Rampa x1 a1 a2) t) - (alturaPeca (Rampa x2 b1 b2) t))) <= 0.2)) 
                                                                                    then (Jogador p d v c (Chao z1)) 
                                                                                    else if (alturaPeca (Rampa x2 b1 b2) t) > (alturaPeca (Rampa x1 a1 a2) t)
                                                                                         then (Jogador p d v c (Morto 1.0)) 
                                                                                         else (Jogador p d v c (Ar (fromIntegral a2) (toGraus (atan((fromIntegral b2)-(fromIntegral b1)))) 0))

alteracaoJogador (Jogador p d v c e) _ _ _ = (Jogador p d v c e)

-- | Função que converte a inclinação de radianos para graus

toGraus :: Double -- ^ Ângulo em radianos
           -> Double -- ^ Ângulo em graus
toGraus a = (180*a)/pi                                                                                         

-- | Função que retorna um jogador quando o estado do jogador é "Morto" (se sim, retorna o jogador sem qualquer alteração) e quando o estado do jogador não é "Morto"

mudaPista :: Jogador -- ^ Representa o jogador escolhido 
             -> Direcao -- ^ Representa a direção atribuida
             -> Int -- ^ Representa a pista do Jogador
             -> Jogador -- ^ Representa o jogador com as devidas alterações

mudaPista j d  x| (estaMorto j == False) && d == C = (Jogador{pistaJogador      = pistaJogador(j)-1 ,
                                                              distanciaJogador  = distanciaJogador j,
                                                              velocidadeJogador = velocidadeJogador j,
                                                              colaJogador       = colaJogador j,
                                                              estadoJogador     = estadoJogador j})  
                
                | (estaMorto j == True) && d == C = (Jogador{pistaJogador      = pistaJogador j,
                                                              distanciaJogador  = distanciaJogador j,
                                                              velocidadeJogador = 0,
                                                              colaJogador       = colaJogador j,
                                                              estadoJogador     = estadoJogador j})
                
                | (estaMorto j == False) && d == B = (Jogador{pistaJogador      = pistaJogador(j)+1 ,
                                                              distanciaJogador  = distanciaJogador j,
                                                              velocidadeJogador = velocidadeJogador j,
                                                              colaJogador       = colaJogador j,
                                                              estadoJogador     = estadoJogador j})
                
                | otherwise =                        (Jogador{pistaJogador      = pistaJogador j,
                                                              distanciaJogador  = distanciaJogador j,
                                                              velocidadeJogador = 0,
                                                              colaJogador       = colaJogador j,
                                                              estadoJogador     = estadoJogador j})

-- | Função que retorna a peça com o jogador

getPeca :: Double -- ^ Representa a distância de um jogador ao inicio
           -> Double -- ^ Este double funciona como um contador para determinar em que pista se quer saber a determinada Peca
           -> Pista -- ^ Representa a pista onde se vai escolher a peça
           -> Peca -- ^ Representa a peça com o jogador

getPeca a z (h:t) = if p == z then h else getPeca a (z + 1.0) t
                 where p = fromIntegral(floor a ) 

-- | Função que muda o estado do jogador se a direção for "Baixo"

mudaPistaCima :: Mapa -- ^ Representa o mapa onde se vão efetuar as alterações 
                 -> Int -- ^ Este Int funciona como um auxiliar para aceder a certo elemento de uma lista
                 -> Direcao -- ^ Representa a direção "Cima"
                 -> [Jogador] -- ^ Representa a lista de jogadores sem as alterações
                 -> [Jogador] -- ^ Representa a lista de jogadores com as alterações

mudaPistaCima m n d [] = []                                                                                                                                            
mudaPistaCima m n d l | n == 1 = [l!!(n-1)] ++ [(mudaPista (alteracaoJogador (l!!n) (getPeca (distanciaJogador (l!!n)) 0.0 (m!!n)) (getPeca (distanciaJogador (l!!n)) 0.0 (m!!(n-1))) (getDecimal(distanciaJogador (l!!n))) ) d (pistaJogador (l!!n))) ]
                      | otherwise = head l: mudaPistaCima m (n-1) d (tail l)    


-- | Função que muda o estado do jogador se a direção for "Baixo"

mudaPistaBaixo :: Mapa -- ^ Representa o mapa onde se vão efetuar as alterações 
                  -> Int -- ^ Este Int funciona como um auxiliar para aceder a certo elemento de uma lista
                  -> Direcao -- ^ Representa a direção "Baixo"
                  -> [Jogador] -- ^ Representa a lista de jogadores sem as alterações
                  -> [Jogador] -- ^ Representa a lista de jogadores com as alterações

mudaPistaBaixo m n d [] = []
mudaPistaBaixo m n d l | n == 0 = [(mudaPista (alteracaoJogador (l!!n) (getPeca (distanciaJogador (l!!n)) 0.0 (m!!n)) (getPeca (distanciaJogador (l!!n)) 0.0 (m!!abs(n+1)))(getDecimal(distanciaJogador (l!!n))) ) d (pistaJogador (l!!n)))] ++ tail l
                       | otherwise = head l : mudaPistaBaixo m (n-1) d (tail l)

-- | Função que retorna a parte decimal do número

getDecimal :: Double -- ^ Número ao qual queremos saber a parte decimal
              -> Double -- ^ Parte decimal

getDecimal x = x - fromIntegral(floor x)


-- | Função que nos diz se o jogador está ou não morto

estaMorto :: Jogador -- ^ Representa o jogador ao qual queremos determinar algo
             -> Bool -- ^ Afirma se o jogador está morto

estaMorto j = estaMortoT (estadoJogador j)

-- | Função que nos diz que um jogador só está morto se o estado do jogador for "Morto"

estaMortoT :: EstadoJogador -- ^ Representa o estado do jogador ao qual queremos atribuir algo
              -> Bool -- ^ Retorna "True" se e só se o estado do jogador é "Morto"

estaMortoT (Chao _) = False
estaMortoT (Morto _) = True
estaMortoT (Ar _ _ _) = False

-- | Função que nos diz que, para acelerar o jogador, este precisa de estar no "Chao"
estaNoChao :: Jogador -- ^ Representa o jogador ao qual queremos determinar algo
              -> Bool -- ^ Afirma se o jogador está no "Chao"

estaNoChao j = estaNoChaoT (estadoJogador j)

-- | Função que nos diz que um jogador só está no chão se o estado do jogador for "Chao"

estaNoChaoT :: EstadoJogador -- ^ Representa o estado do jogador ao qual queremos atribuir algo
               -> Bool -- ^ Retorna "True" se e só se o estado do jogador é "Chao"

estaNoChaoT (Chao _) = True
estaNoChaoT (Morto _) = False
estaNoChaoT (Ar _ _ _) = False

-- | Função que retorna a lista de jogadores quando a jogada é "Acelera"

mudaAcelera :: Int -- ^ Representa o contador de jogadores ao longo da lista de jogadores
               -> [Jogador] -- ^ Representa a lista de jogadores não ordenada
               -> [Jogador] -- ^ Representa a lista de jogadores ordenada

mudaAcelera n [] = []
mudaAcelera n (h:t) | n == 0 = (Jogador{pistaJogador      = pistaJogador h ,
                                        distanciaJogador  = distanciaJogador h,
                                        velocidadeJogador = velocidadeJogador h,
                                        colaJogador       = colaJogador h,
                                        estadoJogador     = mvelA (estadoJogador h)}):t
                    | otherwise = h: mudaAcelera (n-1) t

-- | Função que assegura que o jogador está no chão quando a jogada é "acelera"

mvelA :: EstadoJogador -- ^ Representa um estado de jogador qualquer
         -> EstadoJogador -- ^Representa um estado de jogador quando a jogada é "acelera"
mvelA e = Chao {aceleraJogador = True}

-- | Função que retorna a lista de jogadores quando a jogada é "Desacelera"

mudaDesacelera :: Int -- ^ Representa o contador de jogadores ao longo da lista de jogadores
                  -> [Jogador] -- ^ Representa a lista de jogadores não ordenada
                  -> [Jogador] -- ^ Representa a lista de jogadores ordenada

mudaDesacelera n [] = []
mudaDesacelera n (h:t) | n == 0 = (Jogador{pistaJogador      = pistaJogador h ,
                                        distanciaJogador  = distanciaJogador h,
                                        velocidadeJogador = velocidadeJogador h,
                                        colaJogador       = colaJogador h,
                                        estadoJogador     = mvelD (estadoJogador h)}):t
                    | otherwise = h: mudaDesacelera (n-1) t

-- | Função que assegura que o jogador não está no chão quando a jogada é "Desacelera"

mvelD :: EstadoJogador -- ^ Representa um estado de jogador qualquer
         -> EstadoJogador -- ^Representa um estado de jogador quando a jogada é "Desacelera"
mvelD e = Chao {aceleraJogador = False}

-- | Função que retorna um estado quando a jogada é "Acelera"

funcAcelera :: Int -- ^ Determina o jogador escolhido
               -> Estado -- ^ Representa um estado qualquer
               -> Estado -- ^ Representa um estado com as devidas alterações quando a jogada é "Acelera"

funcAcelera n e = Estado {mapaEstado = mapaEstado e, jogadoresEstado = mudaAcelera n (jogadoresEstado e)}

-- | Função que retorna um estado quando a jogada é "Desacelera"

funcDesacelera :: Int -- ^ Determina o jogador escolhido
                  -> Estado -- ^ Representa um estado qualquer
                  -> Estado -- ^ Representa um estado com as devidas alterações quando a jogada é "Desacelera"

funcDesacelera n e = Estado {mapaEstado = mapaEstado e, jogadoresEstado = mudaDesacelera n (jogadoresEstado e)}

-- | Função que aplica a jogada dispara a um determinado jogador

funcDispara :: Int -- ^ Determina o jogador escolhido
               -> Estado -- ^ Representa um estado qualquer
               -> Estado -- ^ Representa um estado com as devidas alterações quando a jogada é "Dispara"

funcDispara i (Estado m js) | podeDisparar (js!!i) && verificaPrimeiraPeca (js!!i) = Estado (forneceMapaAlterado m (js!!i)) (dispara 0 i js)
                            | otherwise = Estado m js  

-- | Função que percorre a lista de jogadores e retorna a lista de jogadores com as devidas alterações quando a jogada é "Dispara"
dispara :: Int -- ^ Serve de contador
           -> Int -- ^ Representa a posição do jogador
           -> [Jogador] -- ^ Representa uma lista de jogadores qualquer
           -> [Jogador] -- ^ Representa uma lista de jogadores com as alterações caso a jogada ser "Dispara"

dispara contador objetivo (h:t) = if contador == objetivo then (alteraDisparaJogador h) : t 
                                  else h: dispara (contador+1) objetivo t

-- | Função que faz a alteração no jogador

alteraDisparaJogador :: Jogador -- ^ Representa um jogador qualquer
                        -> Jogador -- ^ Representa um jogador alterado

alteraDisparaJogador (Jogador p d v municoes (Chao x)) = if podeDisparar (Jogador p d v (municoes) (Chao x)) then (Jogador p d v (municoes-1) (Chao x)) else (Jogador p d v (municoes) (Chao x))
alteraDisparaJogador (Jogador p d v c e) = (Jogador p d v c e)

-- | Função que impede que as munições sejam negativas

podeDisparar :: Jogador -- ^ Representa um jogador ao qual queremos atribuir algo
                -> Bool -- ^ Retorna "True" se as munições forem 0,1,2,3,4 ou 5

podeDisparar (Jogador p d v (municoes) e) = if (municoes <= 5 && municoes > 0 && d > 1) then True else False

-- | Função que percorre o mapa e dá a peça alterada

forneceMapaAlterado:: Mapa -- ^ Representa um mapa qualquer
                      -> Jogador -- Representa um jogador ao qual queremos fazer alterações
                      -> Mapa -- ^ Retorna um mapa com o jogador ao qual queremos fazer alterações

forneceMapaAlterado (h:t) (Jogador 0 d v c e) = alteraPecaDispara h (floor(d)-1):t
forneceMapaAlterado (h:t) (Jogador p d v c e) = h:forneceMapaAlterado t (Jogador (p-1) d v c e)

-- | Função que dá a pista com a peça alterada

alteraPecaDispara :: Pista -- ^ Representa uma pista qualquer
                     -> Int -- ^ Representa um contador
                     -> Pista -- ^ Retorna uma pista com a peça alterada

alteraPecaDispara (h:t) 0 = mudaPiso h : t
alteraPecaDispara (h:t) x = h: alteraPecaDispara t (x-1)

-- | Função que impede a primeira peça ser de piso cola

verificaPrimeiraPeca :: Jogador -- ^ Representa um jogador qualquer
                        -> Bool -- ^ Retorna "True" se o jogador não estiver na primeira peça

verificaPrimeiraPeca (Jogador p d v c e) = if d >= 1 then True else False 

-- | Função que muda o piso da peça

mudaPiso :: Peca -- ^ Representa uma peça qualquer
            -> Peca -- ^ Representa a peça com o piso Cola

mudaPiso (Recta x a) = Recta Cola a
mudaPiso (Rampa x a1 a2) = Rampa Cola a1 a2 







