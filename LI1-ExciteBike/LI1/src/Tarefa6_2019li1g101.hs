{- |
	
	= Introdução

	Na Tarefa6 foi nos pedido para criar um bot(jogador artificial), capaz de jogar "EXCITE BIKE" sozinho, sem precisar de 
	nenhum in-put humano. Nesta Tarefa usamos também as funções principais de outras tarefas para que o jogador possa então 
	realizar todo o tipo de movimentos previamente implementados ao longo de todas as outras tarefas.
	Esta Tarefa implica muito trabalho de suposição e de pensamento a longo prazo, pensamento este que é necessário para perceber
	as mudanças subtis  que um jogador  pode fazer para que este avance mais rápidamente ou para que não tome decisões como Saltar 
	de uma Peça com altura suficiente para que o Jogador fique impedido de de jogar (morto) durante alguns segundos. 

	= Objetivos

	O objetivo foi criar um Jogador artificial que, quando apresentado qualquer mapa, este conseguisse chegar ao fim o mais  rápidamente 
	possivel, ao mesmo tempo que dificule a vida aos outros jogadores (por exemplo lançando disparos de cola);
	De modo a que o jogador consiga completar um jogo com sucesso foram implementadas algumas funções novas usando também outras já 
	existentes de outras tarefas. Como por exemplo, saber se um jogador se  encontra na nossa retaguarda  e se isso for verdade lançar então
	um disparo de cola para que o jogador anterior  fique  retido. Obviamente foi implementado t ambémuma função que contraria a função 
	anterior, se o jogador detetar cola à sua frente este irá mudar de pista, caso  essa mudança nao signifique  resultados piores do que atravessar
	a própria cola (como seria o  exemplo de morrer).
	O  bot rege-se então pela seguinte ordem de trabalhos:

	* Disparar cola caso tenha munições e um jogador se encontre na retaguarda da mesma  pista;
	* Verificar se a peça em frente é segura para viajar;
	* Verificar se existem possiveis peças laterais para o Bot se mover;
	* Verificar se no caso da peça à frente Jogador não for satisfatória, se existe uma melhor peça nas  laterias;

	= Discussão e Conclusão

	Esta Tarefa6, assim como a 5, foi uma tarefa com uma dinâmica diferente da que estavamos habituados pois apresentou nos uma problema novo, que era 
	antecipar jogadas e descobrir como responder a elas. Foi nos pedido um jogador que conseguisse carregar toda a informação  fornecida pelo jogo mais todas 
	as  funcionalidades do jogo para que  este bot funcionasse. Começamos por fazer um bot simplista que pudesse simplesmente andar para a frente. Assim que esse
	processo foi cincluido, passamos a implementar mudanças de pista, e escolher qual dessas  mudanças seria a melhor. De seguida implementamos a função que nos 
	permite disparar cola para dificultara vida a  outros jogadores. Com isto tudo percebmos que com funcionalidades suficientes há a possibilidade de construi um 
	bot que seja completamente imbativel. Por exemplo analisando a pista como um todo e escolher logo, antes de começar, todos os passos a seguir.
	Esta tarefa foi interessante e aproximou nos de uma rede de inteligencia artificial ou de um  assistente artificial, usando comandos(pedidos) e obtendo  resportas
	(jogadas).

-}

module Tarefa6_2019li1g101 where

import LI11920
import Tarefa2_2019li1g101
import Tarefa4_2019li1g101


-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot i e | temCola(j) && verificaJogadoresAtras (jogadoresEstado(e)) j = Just Dispara 
        | otherwise = movimentaJ j (mapaEstado(e))
    where j = jogadoresEstado(e)!!(i)

-- | Função que verifica se o Jogador tem munições de Cola
temCola :: Jogador  -- ^ Jogador atual que se quer verificar se tem ou não munições de cola
        -> Bool  -- ^ Se for True o Jogador tem munições de cola disponiveis
temCola (Jogador _ _ _ c _) = c > 0 

-----------------------------------------------------PEÇA ADJACENTE AO JOGADOR------------------------------------------------------------
-- |Função que dá as peças adjacentes quando o jogador não está nem na primeira nem na ultima pista
findPecaAdjacente :: Mapa            -- ^ Mapa onde se encontra o jogador
                  -> Jogador -- ^ Jogador ao qual queremos saber a sua peça	
                  -> (Peca,Peca)     -- ^ Retorna as peças adjacentes do jogador [peça esquerda,peça direita]
findPecaAdjacente m (Jogador p d _ _ _) = (esq,dir)
                                        where esq = (m!!(p-1)) !! (floor d)
                                              dir = (m!!(p+1)) !! (floor d)

-- |Função que dá as peças adjacentes quando o jogador está na primeira pista
findPecaAdjacenteBaixo :: Mapa        -- ^ Mapa onde se encontra o jogador
                       -> Jogador     -- ^ Jogador ao qual queremos saber a sua peça 
                       -> Peca -- ^ Retorna as peças adjacentes do jogador [peça esquerda,peça direita]
findPecaAdjacenteBaixo m (Jogador p d _ _ _) = (m!!(p+1)) !! (floor d)


-- |Função que dá as peças adjacentes quando o jogador está na ultima pista
findPecaAdjacenteCima :: Mapa        -- ^ Mapa onde se encontra o jogador
                       -> Jogador     -- ^ Jogador ao qual queremos saber a sua peça 
                       -> Peca -- ^ Retorna as peças adjacentes do jogador [peça esquerda,peça direita]
findPecaAdjacenteCima m (Jogador p d _ _ _) = (m!!(p-1)) !! (floor d)

-- |Função que dá as peças adjacentes quando o jogador está na ultima pista
findPROXPeca :: Mapa        -- ^ Mapa onde se encontra o jogador
             -> Jogador     -- ^ Jogador ao qual queremos saber a sua peça 
             -> Peca        -- ^ Retorna a Peça à frente do jogador 
findPROXPeca m (Jogador p d _ _ _) = (m!!p) !! (floor (d+1))

-----------------------------------------------------PRÓXIMAS PEÇAS ADJACENTES AO JOGADOR-------------------------------------------------

-- |Função que dá as peças adjacentes quando o jogador não está nem na primeira nem na ultima pista
findPROXPecaAdjacente :: Mapa            -- ^ Mapa onde se encontra o jogador
                      -> Jogador -- ^ Jogador ao qual queremos saber a sua peça 
                      -> (Peca,Peca)     -- ^ Retorna as peças adjacentes do jogador [peça esquerda,peça direita]
findPROXPecaAdjacente m (Jogador p d _ _ _) = (esq,dir)
                                        where esq = (m!!(p-1)) !! (floor d+1)
                                              dir = (m!!(p+1)) !! (floor d+1)

-- |Função que dá as peças adjacentes quando o jogador está na primeira pista
findPROXPecaAdjacenteBaixo :: Mapa        -- ^ Mapa onde se encontra o jogador
                       -> Jogador     -- ^ Jogador ao qual queremos saber a sua peça 
                       -> Peca -- ^ Retorna as peças adjacentes do jogador [peça esquerda,peça direita]
findPROXPecaAdjacenteBaixo m (Jogador p d _ _ _) = (m!!(p+1)) !! (floor d+1)


-- |Função que dá as peças adjacentes quando o jogador está na ultima pista
findPROXPecaAdjacenteCima :: Mapa        -- ^ Mapa onde se encontra o jogador
                       -> Jogador     -- ^ Jogador ao qual queremos saber a sua peça 
                       -> Peca -- ^ Retorna as peças adjacentes do jogador [peça esquerda,peça direita]
findPROXPecaAdjacenteCima m (Jogador p d _ _ _) = (m!!(p-1)) !! (floor d+1)

------------------------------------------------------------------------------------------------------------------------------------------
-- |Função que dá o atrito de cada peça
atrito :: Peca   -- ^ Peça que se quer saber o atrito
       -> Double -- ^ atrito da peça pedida
atrito (Recta p _)   | p == Terra = 0.25
                     | p == Relva = 0.75
                     | p == Lama = 1.50
                     | p == Boost = -0.50
                     | p == Cola = 3.00

atrito (Rampa p _ _) | p == Terra = 0.25
                     | p == Relva = 0.75
                     | p == Lama = 1.50
                     | p == Boost = -0.50
                     | p == Cola = 3.00

-- |Função que dá o atrito de um piso.
atritoPiso :: Piso   -- ^ Piso que se quer saber o atrito
           -> Double -- ^ atrito do piso pedido
atritoPiso p | p == Terra = 0.25
             | p == Relva = 0.75
             | p == Lama = 1.50
             | p == Boost = -0.50
             | p == Cola = 3.00

-- | Função que dado um tupulo de Peças devolve um tuplo com o piso dessas mesmas peças
pisoPecaEstroides :: (Peca,Peca) -- ^ Tupulo com as Pecas
                  -> (Piso,Piso) -- ^ Tupulo com o piso das pecas
pisoPecaEstroides (a,b) = (x,y)
                    where x = pisoPeca a
                          y = pisoPeca b
------------------------------------------------------------------------------------------------------------------------------------------
-- | Função que recebe um Jogador e um Mapa e verifica as imediações do jogador procurando qual a melhor opção de movimentação fornecidas pelo mapa.
movimentaJ :: Jogador -- ^ Jogador atual
           -> Mapa    -- ^ Mapa atual
           -> Maybe Jogada -- ^ Jogada resultante das  possibilidades de movimentação que o mapa oferece
movimentaJ (Jogador p d v c e) m | prox_peca_piso == Lama || prox_peca_piso == Cola = if (p == 0) then 
                                                                                                    (if (prox_peca_piso == Terra ||  prox_peca_piso == Relva || prox_peca_piso == Boost) 
                                                                                                        then (Just (Movimenta  D))
                                                                                                        else (Just Acelera))
                                                                                                 else if (p == ((length m)-1))
                                                                                                        then if (prox_peca_piso == Terra || prox_peca_piso == Relva|| prox_peca_piso == Boost)  
                                                                                                                then (Just (Movimenta E))
                                                                                                                else (Just Acelera)
                                                                                            else if ((atritoPiso (fst(pisoPecaEstroides(findPecaAdjacente  m (Jogador p d v c e))))) > (atritoPiso (snd(pisoPecaEstroides(findPecaAdjacente  m (Jogador p d v c e))))))
                                                                                                    then (Just (Movimenta D))
                                                                                                    else (Just (Movimenta E)) 
                          | prox_peca_piso == Terra || prox_peca_piso == Relva || prox_peca_piso == Boost = Just Acelera
                          | otherwise = Nothing
                                        where prox_peca = findPROXPeca m (Jogador p d v c e) 
                                              prox_peca_piso = pisoPeca $ prox_peca

-- | Função que  verifica  se na pista do jogador atual existem jogadores à sua retaguarda.
verificaJogadoresAtras :: [Jogador] -- ^ Lista de todos os Jogadores no mapa
                       -> Jogador   -- ^ Jogador atual
                       -> Bool      -- ^ Se for True, existem jogadores da mesma pista na retaguarda do jogador atual
verificaJogadoresAtras [] _ = False
verificaJogadoresAtras (h:hs) j = estaAtras h j && verificaJogadoresAtras hs j

-- | Função que verifica se um jogador se encontra na retaguarda de outro
estaAtras :: Jogador -- ^ outro Jogador 
          -> Jogador -- ^ Jogador atual
          -> Bool    -- ^ Se for true o outro jogador encontra se na retaguarda do Jogador atual
estaAtras (Jogador p d v c e) (Jogador pe de ve ce ee) = p == pe && de > d









