{-|
  
  = Introdução

  O objectivo desta tarefa é calcular o efeito da passagem de um instante de tempo num
  estado do jogo. Os inputs serão um número real positivo que denota o tempo que passou
  desde a última atualização (ou seja, o período de tempo para o qual devem reagir), o 
  mapa do jogo, e o jogador para o qual devem processar o passar do tempo. O resultado 
  deverá ser o jogador após ter sido atualizado. Assume-se que neste instante de tempo 
  o mapa está fixo, i.e., o mapa não sofre alterações por parte de qualquer jogador.
  
  = Objetivos

  O objetivo desta tarefa é atualizar o estado do jogador dado a passagem do tempo.
  Para isso dividimos esta tarefa em 2 partes (funções) que depois se reunem na função 
  principal para gerar a jogada.
  Começamos então por acelerar ou desacelerar o jogador usando a função "acelera", para 
  isso verificamos se o jogador está em condições para acelerar e dependendo do estado do 
  jogador aceleramos ou não.
  Seguidamente pegamos no estado do jogador depois de acelerar, ou seja, depois de passar 
  e ser mudado pela função falada anteriormente, e usamos esse estado para a segunda parte
  desta tarefa que é a função "move".
  Na função pegamos no estado do jogador depois de acelerar e verificamos os parametros em
  que o jogador se pode mover, mesmo que este movimento resulte na sua morte.
  Por fim a função final usa ambas as funções faladas anteriormente.

  = Discussão e Conclusão

  Esta tarefa foi desafiante pois tivemos que ter em conta muitissimos aspetos em relação a 
  todas as partes do mapa e todas os possiveis resultados de movimentos feitos pelo jogador.
  Consideramos que conseguimos resolver o que foi pedido, no entanto faltou nos implementar
  a função que calcula a distancia e a velocidade final quando o Jogador está em queda livre.
  Esta função foi escrita e chegamos mas não pôde ser testada antes do final do prazo de entrega,
  devido ao crash do site,por isso, de modo a não criar erros imprevistos em todas os outros
  testes decidimos não incluir essa ultima função. 


-}

module Tarefa4_2019li1g101 where
import LI11920
import Tarefa2_2019li1g101
import Data.Ratio
 
-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(0.0, mapaTeste1T4, a), (1.0, mapaTeste1T4, a),
            (0.0, mapaTeste1T4, b), (1.0, mapaTeste1T4, b), 
            (0.0, mapaTeste1T4, c), (1.0, mapaTeste1T4, c),
            (0.0, mapaTeste1T4, d), (2.1, mapaTeste1T4, d),
            (0.0, mapaTeste1T4, e), (1.0, mapaTeste1T4, e),
            (0.0, mapaTeste1T4, f), (1.0, mapaTeste1T4, f),
            (0.0, mapaTeste1T4, g), (1.0, mapaTeste1T4, g),
            (0.0, mapaTeste1T4, h), (1.0, mapaTeste1T4, h),
            (0.0, mapaTeste1T4, i), (2.2, mapaTeste1T4, i),
            (3.1, mapaTeste1T4, j), (1.0, mapaTeste1T4, j),
            (0.0, mapaTeste1T4, k), (1.0, mapaTeste1T4, k),
            (0.0, mapaTeste1T4, l), (1.0, mapaTeste1T4, l),
            (0.0, mapaTeste1T4, m), (1.0, mapaTeste1T4, m),
            (0.0, mapaTeste1T4, n), (1.0, mapaTeste1T4, n),
            (0.0, mapaTeste1T4, p), (1.0, mapaTeste1T4, p),
            (0.0, mapaTeste1T4, q), (3.0, mapaTeste1T4, q),
            (0.0, mapaTeste1T4, s), (3.0, mapaTeste1T4, s),
            (0.0, mapaTeste1T4, t), (3.0, mapaTeste1T4, t),
            (0.0, mapaTeste1T4, u), (3.0, mapaTeste1T4, u), (5.0, mapaTeste1T4, u),
            (0.0, mapaTeste1T4, v), (3.0, mapaTeste1T4, v),
            (0.0, mapaTeste1T4, y), (1.0, mapaTeste1T4, y),
            (0.0, mapaTeste1T4, z), (1.0, mapaTeste1T4, z),
            (0.0, mapaTeste1T4, a1), (1.0, mapaTeste1T4, a1),
            (0.0, mapaTeste1T4, a2), (3.0, mapaTeste1T4, a2),
            (0.0, mapaTeste1T4, a3), (1.0, mapaTeste1T4, a3),
            (0.0, mapaTeste1T4, a4), (3.0, mapaTeste1T4, a4),
            (0.0, mapaTeste1T4, a5), (1.0, mapaTeste1T4, a5),
            (0.0, mapaTeste1T4, a6), (1.0, mapaTeste1T4, a6),
            (0.0, mapaTeste1T4, a7), (1.0, mapaTeste1T4, a7),
            (0.0, mapaTeste1T4, a8), (1.0, mapaTeste1T4, a8),
            (0.0, mapaTeste1T4, a9), (1.0, mapaTeste1T4, a9),
            (0.0, mapaTeste1T4, a10), (3.0, mapaTeste1T4, a10),
            (0.0, mapaTeste1T4, a11), (1.0, mapaTeste1T4, a11), (0.5, mapaTeste1T4, a11), (1.5, mapaTeste1T4, a11),
            (0.0, mapaTeste1T4, a12), (1.0, mapaTeste1T4, a12),
            (0.0, mapaTeste1T4, a13), (0.2, mapaTeste1T4, a13),
            (0.0, mapaTeste2T4, a14), (0.7, mapaTeste2T4, a14)
             ]
            where a = Jogador 0 2.5 0 5 (Chao True) 
                  b = Jogador 1 3 0 5 (Chao True)
                  c = Jogador 0 1.5 0 5 (Chao True)
                  d = Jogador 1 2 0 5 (Chao True)
                  e = Jogador 0 3.5 0 5 (Chao True)
                  f = Jogador 1 1.8 0 5 (Chao True)
                  g = Jogador 0 1.1 0 5 (Chao True)
                  h = Jogador 1 1.6 0 5 (Chao True)
                  i = Jogador 0 3.9 0 5 (Ar 4.0 30.1 0.0)
                  j = Jogador 1 3.9 0 5 (Ar 4.0 30.1 1.0)
                  k = Jogador 0 0.1 0 5 (Chao True)
                  l = Jogador 1 0.1 0 5 (Chao True) 
                  m = Jogador 0 0 1.2 5 (Chao True)
                  n = Jogador 0 1 1 0 (Chao False)
                  p = Jogador 1 1.9 1.9 2 (Chao False)
                  q = Jogador 0 0.5 0.5 1 (Chao True)
                  s = Jogador 0 0 0 5 (Morto 1.0)
                  t = Jogador 0 1 0 0 (Morto 1.0)
                  u = Jogador 0 0.5 0 1 (Morto 1.0)
                  v = Jogador 1 1.9 0 2 (Morto 1.0)
                  y = Jogador 0 1.3 1 0 (Ar 1 74.0 9.8) 
                  z = Jogador 0 0.5 0.5 1 (Ar 0.6 (-74.0) 2.0)
                  a1 = Jogador 1 1.9 1.9 2 (Ar 1.9 (-40.5) 1.8)
                  a2 = Jogador 0 0.5 0.5 1 (Ar 0.6 15.0 1.7)
                  a3 = Jogador 1 1.9 1.9 2 (Ar 1.9 40.5 2.1)
                  a4 = Jogador 0 2.5 0.5 1 (Ar 3.6 30.1 0.0)
                  a5 = Jogador 1 3.7 1.9 2 (Ar 1.9 (-15.0) 0.0)
                  a6 = Jogador 1 1.4 0 5 (Chao True)
                  a7 = Jogador 0 1 1 0 (Chao False)
                  a8 = Jogador 1 2.5 0 5 (Chao True)
                  a9 = Jogador 0 1 1 0 (Chao False)
                  a10 = Jogador 1 2.5 0 5 (Morto 1.0)
                  a11 = Jogador 0 1 1 0 (Chao False)
                  a12 = Jogador 1 1.2 5 5 (Chao True)
                  a13 = Jogador 1 1.8 5 5 (Chao True)
                  a14 = Jogador 0 1.8 5 5 (Chao True)

-- | Mapa de teste 1
mapaTeste1T4 :: Mapa
mapaTeste1T4 = [[Recta Terra 0, Rampa Relva 0 1, Recta Cola 1, Rampa Boost 1 3],[Recta Terra 0, Rampa Lama 0 1, Recta Lama 1, Rampa Boost 1 0]]

-- | Mapa de teste 2             
mapaTeste2T4 :: Mapa
mapaTeste2T4 = [[Recta Terra 0, Rampa Lama 0 1, Recta Lama 1, Rampa Cola 1 0]]
 
-- * Funções principais da Tarefa 4.
 
-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
      -> Mapa    -- ^ O mapa utilizado.
      -> Jogador -- ^ O estado anterior do 'Jogador'.
      -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)
 
-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
           -> Mapa    -- ^ O mapa utilizado.
           -> Jogador -- ^ O estado anterior do 'Jogador'.
           -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera t m j | estaNoChao j = j { velocidadeJogador = atualizaVelocidade_Chao j m t}
               | estaMorto j = j
               | verificaAr j = j { velocidadeJogador = atualizaVelocidade_Ar j t , 
                                    estadoJogador = Ar  { alturaJogador = altAnt (devolveEst j), 
                                                          inclinacaoJogador = incAnt (devolveEst j), 
                                                          gravidadeJogador = calculaGravAt_Ar j t } 
                                                        } 
               
-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
      -> Mapa    -- ^ O mapa utilizado.
      -> Jogador -- ^ O estado anterior do 'Jogador'.
      -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
 
move 0 _ j = j     
move t m (Jogador p d v c e) | estaMorto (Jogador p d v c e) = mudaPos_Morto t (Jogador p d v c e) 
                             | verificaAr (Jogador p d v c e) = move_Ar t (Jogador p d v c e) m
                             | semVelocidade (Jogador p d v c e) = (Jogador p d v c e)
                             | estaNoChao (Jogador p d v c e) = move_Chao t (Jogador p d v c e) m (findPeca m (Jogador p d v c e)) ((m!!(p))!!(ceiling d))

--------------------------------------------------------------------------------------------------------------------------
-- | Testa se um jogador tem velocidade para fazer o movimento
semVelocidade :: Jogador -> Bool
semVelocidade (Jogador _ _ v _ _) | v == 0    = True
                                  | otherwise = False
-- | Função que altera a velocidade do jogador caso este esteja no chão
atualizaVelocidade_Chao :: Jogador -- ^ Representa o jogador a considerar
                                 -> Mapa -- ^ Representa o mapa onde se encontra o jogador
                                 -> Double -- ^ Representa o tempo decorrido 
                                 -> Double -- ^ Representa a velocidade atualizada qaundo o jogador está no chão
atualizaVelocidade_Chao (Jogador p d v c e) mapa t = if a < 0 then 0 else a 
                                                          where a = v + (fromIntegral (accelMota v e)- ( (trocaAtritoPiso (pisoPeca(findPeca mapa (Jogador p d v c e))) * v))) * t
 
-- | Função que atribui o atrito consoante o piso da peça
trocaAtritoPiso :: Piso -- ^ Piso da peça que vai determinar o valor do atrito
                    -> Double -- ^ Valor do atrito
 
trocaAtritoPiso x | x == Terra = 0.25
                  | x == Relva = 0.75
                  | x == Lama = 1.50
                  | x == Boost = -0.50
                  | x == Cola = 3.00
 
-- | Função que indica se o jogador está no chão e a acelerar
accelJogador :: EstadoJogador -- ^ Estado do jogador ao qual queremos saber se está no chão a acelerar
                 -> Bool -- ^ Retorna True se o jogador está no chão a acelerar, caso contrário retorna False
accelJogador e = if estaNoChaoT e && aceleraJogador e then True else False
 
-- | Função que indica a aceleração da mota
accelMota :: Double -- ^ Representa a velocidade inicial do jogador
              -> EstadoJogador -- ^ Representa o estado do jogador
              -> Int -- ^ Representa a aceleração do jogador 
accelMota v e = if (v < 2 && accelJogador e) then 1 else 0 
             
-- | Função que nos dá a peça do jogador
findPeca :: Mapa -- ^ Mapa onde se encontra o jogador
            -> Jogador -- ^ Jogador ao qual queremos saber a sua peça
            -> Peca -- ^ Retorna a peça do jogador
findPeca m (Jogador p d _ _ _) = (m!!p) !! (floor d)
----------------------------------------------------------------------------------------
-- | Função que retorna apenas o piso da peça
pisoPeca :: Peca -- ^ Peça do jogador
             -> Piso -- ^ Piso da peça do jogador
pisoPeca (Recta x _) = x
pisoPeca (Rampa x _ _) = x
 
 --------------------------------------------------------------------------------------------------------------------------
-- | Função que atualiza a velocidade do jogador caso este esteja no ar, afetada pela gravidade
atualizaVelocidade_Ar :: Jogador -- ^ Representa o jogador a considerar
                                   -> Double -- ^ Representa o tempo decorrido
                                   -> Double -- ^ Representa a velocidade atualizada quando o jogador está no ar, afetada pela gravidade
atualizaVelocidade_Ar (Jogador _ _ v _ _) t = if b < 0 then 0 else b
                                               where   
                                               b = v - (resistenciaAr * v * t)
                                               resistenciaAr = 0.125
 
-- | Função que calcula a gravidade do jogador atualizada
calculaGravAt_Ar :: Jogador -- ^  Representa o jogador a considerar
                         -> Double -- ^ Representa o tempo decorrido
                         -> Double -- ^ Representa a velocidade do jogador afetada pela gravidade
calculaGravAt_Ar (Jogador _ _ _ _ (Ar _ _ g)) t = g + accelGravidade * t
                                                   where
                                                   accelGravidade = 1.0 
 
-- | Função que verifica se o jogador está no ar
verificaAr :: Jogador -- ^ Representa o jogador ao qual queremos determinar algo
               -> Bool -- ^ Retorna True se o jogador estiver no "ar"
verificaAr j = verificaArT (estadoJogador j)
 
-- | Função que apenas retorna o estado "Ar"
verificaArT :: EstadoJogador -- ^ Representa um estado do jogador qualquer
                -> Bool -- ^ Retorna True se o estado for "ar"
verificaArT (Chao _) = False
verificaArT (Morto _) = False
verificaArT (Ar _ _ _) = True 
 
-- | Função que retorna a altura do jogador anterior quando está no ar
altAnt :: EstadoJogador  -- ^ Representa o estado do jogador
           -> Double -- ^ Retorna a sua altura
altAnt (Ar aj _ _) = aj
 
-- | Função que retorna a inclinação do jogador anterior quando está no ar
incAnt :: EstadoJogador -- ^ Representa o estado do jogador
           -> Double -- ^ Retorna a sua inclinaçao
incAnt (Ar _ ij _) = ij
 
-- | Função que retorna o estado do jogador
devolveEst :: Jogador -- ^ Representa o jogador ao qual queremos saber o seu estado
               -> EstadoJogador -- ^ Retorna apenas o seu estado
devolveEst (Jogador _ _ _ _ e) = e 
 ---------------------------------------------------------------------------------------------------------------------------
 
-- | Função que movimenta o jogador quando está morto
mudaPos_Morto :: Double -- ^ Tempo decorrido
                   -> Jogador -- ^ Jogador com o estado "Morto"
                   -> Jogador -- ^ Jogador alterado 
mudaPos_Morto t j = if (a > 0.0) then j { estadoJogador = Morto { timeoutJogador = a } } else j { estadoJogador = Chao { aceleraJogador = False }, velocidadeJogador = 0.0 }  
                     where a = ((timeoutJogador(estadoJogador j)) - t) 
----------------------------------------------------------------------------------------------------------------------------
-- | Função que movimenta o jogador quando está no chão
move_Chao :: Double -- ^ Tempo decorrido
              -> Jogador -- ^ Jogador inicial com o estado "Chao"
              -> Mapa -- ^ Mapa onde se encontra o jogador
              -> Peca -- ^ Peça onde o jogador se encontra atualmente
              -> Peca -- ^ Proxima peça do jogador
              -> Jogador -- ^ Jogador alterado 
 
move_Chao t (Jogador p d v c e) m peca1 peca2 |verificaDPI (findPeca m (Jogador p d v c e)) t (Jogador p d v c e)   = (Jogador p (d + v * t * cos (ang)) v c e) 
                                              |((findInclin peca2) >= (findInclin peca1))                           = result_1
                                              |((findInclin peca2) < (findInclin peca1))                            = result_2
                                               
                                               
                                               where 
                                               x1 = (findAlturaJogador peca1 peca2) 
                                               x2 = findInclin (findPeca m (Jogador p d v c e)) * (180/pi)
                                               result_1 = (Jogador p (aproximaD (Jogador p d v c e)) v c (Chao (estAnterior e)))
                                               result_2 = (Jogador p (aproximaD (Jogador p d v c e)) v c (Ar x1 x2 0.0))
                                               ang = findInclin (findPeca m (Jogador p d v c e))
  
-- | Função que determina se ele percorre uma peça inteira
verificaDPI :: Peca -- ^ Peça onde se encontra o jogador
               -> Double -- ^ Representa o tempo decorrido 
               -> Jogador -- ^ Representa o jogador
               -> Bool -- ^ Retorna True se o jogador não tiver percorrido a peça inteira 
verificaDPI peca t (Jogador p d v c e) = (distPercPeca peca t (Jogador p d v c e) < fromIntegral(ceiling d))    
                                                                                    
-- | Função que nos dá o comprimento da rampa
dPeca :: Peca -- ^ Representa a peça do jogador
         -> Double -- ~ Representa o comprimento da rampa 
dPeca (Rampa ps a1 a2) | a1 < a2 = fromIntegral (a2 - a1) * sin (ang)   
                       | a1 > a2 = fromIntegral (a1 - a2) * sin (ang)
                       where ang = findInclin (Rampa ps a1 a2) 
 
 
-- | Função que nos dá a distancia percorrida numa peça inteira
distPercPeca :: Peca -- ^ Representa a peça do jogador
                -> Double -- ^ Representa o tempo decorrido
                -> Jogador -- ^ Representa o jogador 
                -> Double -- ^ Representa a distancia percorrida na totalidade da peça
 
distPercPeca (Recta _ a) t (Jogador p d v c e) = d + v * t                                 
distPercPeca (Rampa ps a1 a2) t (Jogador p d v c e) |getDecimal d == 0 = getDecimal(d) + v * t * cos (ang)
                                                    |otherwise         = (d) + v * t * cos (ang)
                                                      where ang = findInclin (Rampa ps a1 a2) 
 
 
-- | Função que retorna o estado do jogador na peça anterior
estAnterior :: EstadoJogador -- ^ Representa o estado do jogador
               -> Bool -- ^ Retorna "True" se e só se o jogador estiver no "Chao", não importando se está a acelerar ou não
estAnterior e = if e == Chao True || e == Chao False then True else False                               
 
-- | Função que descobre a inclinação da peça
findInclin :: Peca -- ^ Representa a peça no qual queremos saber a sua inclinação
               -> Double -- ^ Representa a inclinação da peça em questão
findInclin (Recta _ a) = 0.0
findInclin (Rampa _ a1 a2)  |a1 > a2   = (atan (ax-ay))
                            |otherwise = (atan (ay-ax))
                           where ax = fromIntegral a1 :: Double
                                 ay = fromIntegral a2 :: Double
 
-- | Função que descobre a altura do jogador no inicio da segunda peça 
findAlturaJogador :: Peca -- ^ Representa a peça anterior
                     -> Peca -- ^ Representa a peça atual
                     -> Double -- ^ Representa a altura da segunda peça
findAlturaJogador (Recta _ _)   (Recta _ a1)   = fromIntegral a1
findAlturaJogador (Rampa _ _ _) (Recta _ a)    = fromIntegral a 
findAlturaJogador (Recta _ _)   (Rampa _ a1 _) = fromIntegral a1
findAlturaJogador (Rampa _ _ _) (Rampa _ a3 _) = fromIntegral a3
 -----------------------------------------------------------------------------------------------------------------------------------
-- | Função que movimenta o jogador quando ele está no ar
move_Ar :: Double -- ^ Tempo decorrido
           -> Jogador -- ^ Representa o jogador no ar
           -> Mapa -- ^ Representa o mapa do jogador
           -> Jogador -- ^ Representa o jogador com as alterações da inclinação e velocidade
                                                                                                          
move_Ar t (Jogador p d v c (Ar aj ij gj)) m |verificaDPIar (pecaJog) t (Jogador p d v c (Ar aj ij gj)) = (Jogador p d v c (Ar ((aj + sin (ang) * v * t - (gj * t))) ij gj))
                                            |(ij - (ang1) >= 45.0) = (Jogador p d 0.0 c (Morto 1.0))  
                                            |(ij - (ang1) < 45.0)  = (Jogador p d v c (Chao False))    
                                           
   where 
 
   pecaJog = findPeca m (Jogador p d v c (Ar aj ij gj))
   ang = findInclin (pecaJog) 
   ang1 = (findInclin (pecaJog)) * (180/pi)

-- | Função que verifica se o jogador percorre a peça inteira
verificaDPIar :: Peca -- ^ Peça onde se encontra o jogador
               -> Double -- ^ Representa o tempo decorrido 
               -> Jogador -- ^ Representa o jogador
               -> Bool -- ^ Retorna True se o jogador não tiver percorrido a peça inteira 
verificaDPIar peca t (Jogador p d v c e) = True   
                                                                                    
-- | Função que nos dá o comprimento da rampa
dPecaar :: Peca    -- ^ Representa a peça do jogador
         -> Double -- ~ Representa o comprimento da rampa 
dPecaar (Rampa ps a1 a2) | a1 < a2 = fromIntegral (a2 - a1) * sin (ang)   
                       | a1 > a2 = fromIntegral (a1 - a2) * sin (ang)
                       where ang = findInclin (Rampa ps a1 a2) 
 
 
-- | Função que nos dá a distancia percorrida numa peça inteira
distPercPecaar :: Peca     -- ^ Representa a peça do jogador
                -> Double  -- ^ Representa o tempo decorrido
                -> Jogador -- ^ Representa o jogador 
                -> Double  -- ^ Representa a distancia percorrida na totalidade da peça
 
distPercPecaar (Recta _ a) t (Jogador p d v c e) = d + v * t                              
distPercPecaar (Rampa ps a1 a2) t (Jogador p d v c e) |getDecimal d == 0 = getDecimal(d) + v * t * cos (ang)
                                                      |otherwise         = (d) + v * t * cos (ang)
                                                      where ang = findInclin (Rampa ps a1 a2) 

-- | Função que trata da distancia percorrida
aproximaD :: Jogador -- ^ Dá a distancia do jogador
             -> Double -- ^ Dá o ceiling d 
aproximaD (Jogador p d v c e) | d == fromIntegral (ceiling d) = (d+1)
                              | otherwise = fromIntegral (ceiling d)
