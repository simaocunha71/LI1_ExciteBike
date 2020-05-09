{- |
= Introdução
Na tarefa 5, tivemos o objetivo de recriar graficamente a nossa versão do "ExciteBike" recorrendo à biblioteca Gloss do Haskell.

= Objetivos

O nosso principal objetivo era criar um jogo com o design clássico dos jogos arcade, mas com aspeto apelativo para o utilizador.

Decidimos usar imagens que representassem de forma ideal cada objeto, como as retas, as rampas, a plateia a assistir à corrida e o próprio jogador.

Escolhemos um tema retro para o nosso jogo, uma vez que somos fãs do tema "SuperMario".

Todas as imagens que aparecem no nosso jogo foram criadas por nós ou retiradas diretamente de videojogos através da captura de ecrã e devida manipulação no PhotoShop, 
pelo que a violação dos direitos de autor não será um problema para o nosso trabalho.

= Discussão e conclusão

O nosso jogo não ficou o ideal devido a vários fatores: 
-não conseguimos implementar rampas com altura igual ou superior a 2 unidades;
-não conseguimos tornar o jogo de modo a poderem jogar 2 ou mais pessoas; 
-não conseguimos colocar o jogador a deslocar-se conforme a pista;
-gostaríamos de ter criado um menu que permitisse ao utilizador criar um mapa ao seu gosto e escolher outras personagens para participar na corrida.

O processo de obtenção das imagens das peças começou com uma imagem quadrada (para as retas) e com uma imagem triangular (para as rampas), 
mas terminou com o aspeto final de uma linha horizontal e oblíqua. Tivemos alguma dificuldade em ajustar as imagens de modo a ficar uma pista semelhante à realidade, nomeadamente ao utilizar o "Scale", "Translate" e o "Rotate".
Por causa disto, decidimos utilizar as imagens onde cada peça tem apenas o piso. Além disto, como não conseguimos implementar mapas aleatorios, decidimos ter apenas um mapa fixo.


Apesar dos problemas enumerados em cima, gostamos da forma como o nosso jogo ficou. 

Foi interessante desenhar graficamente a nossa versão do "ExciteBike" com o Gloss. Pensamos que é uma boa ferramenta para iniciantes na programação, pois permite-nos perceber os mecanismos associados à arte de programar.

O maior obstáculo foi aprender a usar o Gloss, uma vez que foi isso que nos impediu de implementar mais ideias ao nosso jogo.

Para concluir, consideramos que esta tarefa foi interessante e didática, já que podíamos criar algo criativo e aprender um pouco mais sobre o Haskell. 

-}

module Main where

import LI11920
import Tarefa1_2019li1g101
import Tarefa2_2019li1g101 
import Tarefa4_2019li1g101  
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- | Função principal da Tarefa 5.

-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.

type EstadoGloss = (Float,Float, Estado, [Figura],(Maybe Key))

-- | Este tipo de dados serve para ajudar na associação de imagens referente a peças, a jogadores e ao fundo 
data Figura = FiguraPec {pic :: Picture , peca :: Peca } 
            | FiguraJog {pic :: Picture , numPista :: Int , x :: Int, y :: Int}
            | FiguraFundo {pic :: Picture }  
              deriving (Show,Eq) 

-- | Função que nos dá o estado inicial do jogo, ou seja, retorna a posição das peças a desenhar no mapa ao iniciar o jogo
estadoGlossInicial :: [Figura] -- ^ Representa uma lista de peças às quais podem ser retas ou rampas com um determinado piso
                      -> EstadoGloss -- ^ Representa o estado inicial do jogo
estadoGlossInicial li = (-1775,0,estadoI,li,Nothing)

-- | Construtor que representa o estado inicial 
estadoI :: Estado
estadoI = (Estado m ([Jogador 0 0 0 5 (Chao False)]))

-- | Construtor que representa o valor do framerate
fr :: Int
fr = 50

-- | Construtor que representa a janela (neste caso é em "Tela cheia")
dm :: Display
dm = FullScreen 

-- | Função que aplica a função "passo" da Tarefa4 a todos os jogadores.
passoTodos :: Float -- ^ REpresenta o tempo decorrido
              -> Mapa -- ^ Representa o mapa onde vão ocorrendo as alterações 
              -> [Jogador] -- ^ Representa a lista de jogadores sem as alterações provocadas pela "passo"
              -> [Jogador] -- ^ Representa a lista de jogadores alterados
passoTodos _ _ [] = []
passoTodos n m (h:t) = (passo (realToFrac n) m h ): passoTodos n m t  

-- | Função que representa o que acontece a um estadoGLoss consoante a passagem do tempo
reageTempoGloss :: Float -- ^ Representa o valor do tempo decorrido
                   -> EstadoGloss -- ^ Representa o estadoGloss sem a alteração
                   -> EstadoGloss -- ^ Representa o estadoGLoss alterado com a passagem do tempo

reageTempoGloss n (x,y,(Estado m js),li,Just (SpecialKey KeyRight)) | verificaAr (head js) = (x,y,newe {jogadoresEstado = epasso},listaAtualizada,Just (SpecialKey KeyRight))
                                                                    | otherwise = (x,y,newe_1 {jogadoresEstado = epasso},listaAtualizada_1,Just (SpecialKey KeyRight))
                                                                    where 
                                                                    newe = jogada 0 (Movimenta D) (Estado m js)
                                                                    newe_1 = jogada 0 (Acelera) (Estado m js)
                                                                    epasso = passoTodos n m (jogadoresEstado newe)
                                                                    epasso_1 = passoTodos n m(jogadoresEstado newe_1)
                                                                    listaAtualizada = atualizaFigurasJogadores li (Movimenta D) (head js)
                                                                    listaAtualizada_1 = atualizaFigurasJogadores li (Acelera) (head js)



reageTempoGloss n (x,y,(Estado m js),li,Just (SpecialKey KeyLeft))  | verificaAr (head js) =  (x,y,newe {jogadoresEstado = epasso},listaAtualizada,Just (SpecialKey KeyLeft))
                                                                    | otherwise = (x,y,newe_2 {jogadoresEstado = epasso},listaAtualizada_1,Just (SpecialKey KeyLeft))
                                                                    where 
                                                                    newe = jogada 0 (Movimenta E) (Estado m js)
                                                                    newe_2 = jogada 0 (Desacelera) (Estado m js)
                                                                    epasso =  passoTodos n m (jogadoresEstado newe)
                                                                    epasso_2 = passoTodos n m (jogadoresEstado newe_2)
                                                                    listaAtualizada = atualizaFigurasJogadores li (Movimenta E) (head js)
                                                                    listaAtualizada_1 = atualizaFigurasJogadores li (Desacelera) (head js)

reageTempoGloss n (x,y,(Estado m js),li,Just (SpecialKey KeyUp)) = (x,y,newe {jogadoresEstado = epasso},listaAtualizada,Just (SpecialKey KeyUp))
                                                                   where 
                                                                   newe = jogada 0 (Movimenta C) (Estado m js)
                                                                   epasso =  passoTodos n m(jogadoresEstado newe)
                                                                   listaAtualizada = atualizaFigurasJogadores li (Movimenta C) (head js)


reageTempoGloss n (x,y,(Estado m js),li,Just (SpecialKey KeyDown))  = (x,y,newe {jogadoresEstado = epasso}, (listaAtualizada),Just (SpecialKey KeyDown))
                                                                      where 
                                                                      newe = jogada 0 (Movimenta B) (Estado m js)
                                                                      epasso =  passoTodos n m(jogadoresEstado newe)
                                                                      listaAtualizada = atualizaFigurasJogadores li (Movimenta B) (head js)

reageTempoGloss n (x,y,(Estado m js),li,Just (Char 'p'))  = (x,y,newe {jogadoresEstado = epasso}, (listaAtualizada),Just (Char 'p'))
                                                            where 
                                                            newe = jogada 0 (Dispara) (Estado m js)
                                                            epasso =  passoTodos n m (jogadoresEstado newe)
                                                            listaAtualizada = atualizaFigurasJogadores li (Dispara) (head js)

reageTempoGloss _ s = s 

-- | Função que move a imagem do jogador
atualizaFigurasJogadores :: [Figura] -- ^ Representa a lista de figuras na qual queremos só as referentes às de jogadores
                            -> Jogada -- ^ Representa a jogada do jogador 
                            -> Jogador -- ^ Representa o jogador no qual queremos que a respetiva imagem se mova
                            -> [Figura] -- ^ Representa a lista de imagens com as devidas alterações 
atualizaFigurasJogadores [] _ _ = []

atualizaFigurasJogadores ((FiguraJog pic n x y):t) (Movimenta D) jgd = (FiguraJog (Rotate inclinacaoPecaPic pic) n (x+1) y) : t
                                                                       where inclinacaoPecaPic = (realToFrac(findInclin (findPeca m jgd)))

atualizaFigurasJogadores ((FiguraJog pic n x y):t) (Acelera) jgd     = (FiguraJog  pic n (x+1) y) : t

atualizaFigurasJogadores ((FiguraJog pic n x y):t) (Movimenta E) jgd = (FiguraJog (Rotate inclinacaoPecaPic pic) n (x-1) y) : t
                                                                       where inclinacaoPecaPic = (realToFrac(findInclin (findPeca m jgd)))

atualizaFigurasJogadores ((FiguraJog pic n x y):t) (Desacelera) jgd  = (FiguraJog  pic n (x-1) y) : t

atualizaFigurasJogadores ((FiguraJog pic n x y):t) (Movimenta C) jgd = (FiguraJog pic n x (y+1)) : t

atualizaFigurasJogadores ((FiguraJog pic n x y):t) (Movimenta B) jgd = (FiguraJog pic n x (y-1)) : t

atualizaFigurasJogadores ((FiguraJog pic n x y):t) (Dispara) jgd     = (FiguraJog pic n x y) : t -- piso da peça anterior muda para cola

atualizaFigurasJogadores (h:t) x js = h : (atualizaFigurasJogadores t x js )

-- | Função que, dado um evento (por exemplo, clicar numa tecla), move a peça 
reageEventoGloss :: Event -- ^ Representa o evento atribuido
                    -> EstadoGloss -- ^ Representa um estadoGloss qualquer
                    -> EstadoGloss -- ^ Representa um estadoGloss alterado de acordo com o evento
reageEventoGloss (EventKey k Down _ _) (x,y,estado,li,_) = (x,y,estado,li,Just k)
reageEventoGloss (EventKey k Up _ _) (x,y,estado,li,Just t) | k == t = (x,y,estado,li,Nothing)
                                                            | otherwise = (x,y,estado,li,Just t)
reageEventoGloss _ s = s 

-- | Função que coloca as figuras numa dada posição 
desenhaEstadoGloss :: EstadoGloss -- ^ Representa o estadoGloss com as peças a desenhar
                      -> Picture -- ^ Retorna as imagens das peças a desenhar numa certa coordenada
desenhaEstadoGloss (x,y,(Estado m js),li,_) = pictures (desenharAPista ++ [desenhaFundo] ++ desenhaJogadores)
                                                  where desenharAPista = percorrePistas m ((x-100),y,(Estado m js),li,Nothing)
                                                        desenhaFundo = Translate 0 350 (Scale 1.75 1.75 (encontraFundo li))
                                                        desenhaJogadores = (encontraJogadores li)

-- | Função que percorre uma mapa e retorna uma pista com as imagens das suas peças constituintes
percorrePistas :: Mapa -- ^ Representa o mapa a desenhar
                  -> EstadoGloss -- ^ Representa um estadoGloss qualquer
                  -> [Picture] -- ^ Representa a pista com as imagens das peças constituintes
percorrePistas [] _ = []
percorrePistas m (x,y,e,li,Nothing) =  (percorrePista (head m) (x,y,estadoI,li,Nothing)) ++ percorrePistas (tail m) (x,(y-100),e,li,Nothing)

-- | Função que percorre uma pista e retorna uma lista de imagens com as peças constituintes
percorrePista :: Pista -- ^ Representa uma pista a desenhar
                 -> EstadoGloss -- ^ Representa um estadoGloss qualquer
                 -> [Picture] -- ^ Retorna as imagens das peças a desenhar numa certa coordenada
percorrePista [] _ = []
percorrePista pista (x,y,(Estado m js),li,_) = ((Scale 0.45 0.45 (Translate x y p)) : percorrePista (tail pista) ((x+535), y, (Estado m js), li, Nothing))
                           where p = corrigePosicao( obtemFigura (head pista) li ) (head pista)

-- | Função que retorna a imagem correspondente a uma determinada peça
obtemFigura :: Peca -- ^ Representa uma peça a desenhar
               -> [Figura] -- ^ Representa a lista de imagens a comparar com a peça
               -> Picture -- ^ Representa a imagem correspondente à peça
obtemFigura p [] = Blank             
obtemFigura peca ((FiguraPec picture peca1):t) = if saoiguais peca peca1 then Translate 0 1 picture  else obtemFigura peca t 
obtemFigura peca  ((FiguraJog picture n _ _):t)  = obtemFigura peca t 
obtemFigura peca  ((FiguraFundo picture):t)  = obtemFigura peca t 

-- | Função que ajusta as imagens (na sua altura, etc)
corrigePosicao :: Picture -- ^ Representa a imagem da peça
                  -> Peca -- ^ Representa a peça ao qual queremos recriar na imagem
                  -> Picture -- ^ Representa a imagem alterada
corrigePosicao pic (Recta p a) = if a == 1 then Translate 0 1 pic else pic 
corrigePosicao pic (Rampa p a1 a2) = pic

-- | Função que compara duas peças e retorna True se forem ambas retas ou rampas e se tiverem o mesmo piso
saoiguais :: Peca -- ^ Representa uma peça qualquer
             -> Peca -- ^ Representa uma peça qualquer
             -> Bool -- ^ Retorna True se forem ambas retas ou rampas e se tiverem o mesmo piso
saoiguais (Recta p a)(Recta p1 a1) = if (p == p1) && (a == a1) then True else False
saoiguais (Rampa p a1 a2) (Rampa p1 b1 b2) = if p == p1 && (((a1 > a2) && (b1>b2)) || ((a1<a2) && (b1<b2))) then True else False 
saoiguais _ _ = False 

-- | Função que serve para desenhar o fundo do jogo
encontraFundo :: [Figura] -- ^ Representa a lista de imagens no jogo
                  -> Picture -- ^ Representa a lista de imagens correspondentes ao fundo
encontraFundo [] = Blank 
encontraFundo ((FiguraFundo pic):t) = pic
encontraFundo ((FiguraPec pic peca):t) = encontraFundo t  
encontraFundo ((FiguraJog pic primPista _ _):t) = encontraFundo t  

-- | Função que serve para desenhar os jogadores
encontraJogadores :: [Figura] -- ^ Representa a lista de imagens no jogo
                     -> [Picture] -- ^ Representa a lista de imagens correspondentes a jogadores
encontraJogadores [] = [] 
encontraJogadores ((FiguraJog pic n x y):t) = (Translate (realToFrac ((-935)+x*10)) (realToFrac((-60 - n * 45)+y*10)) (Scale 0.15 0.15 pic) ) : encontraJogadores t  
encontraJogadores ((FiguraFundo pic):t) = encontraJogadores t  
encontraJogadores ((FiguraPec pic _):t) = encontraJogadores t  

--------------------------------------------------------------------------------------------------------------------------------------------

-- | Função que possui a função "play" que vai permitir que o jogo possa decorrer
main :: IO ()
main = do 
    marioK      <- loadBMP "Images/marioK.bmp"
    retaRelva   <- loadBMP "Images/retaRelva.bmp"
    retaTerra   <- loadBMP "Images/retaTerra.bmp"
    retaLama    <- loadBMP "Images/retaLama.bmp"
    retaCola    <- loadBMP "Images/retaCola.bmp"
    retaBoost   <- loadBMP "Images/retaBoost.bmp"
    rampaSTerra <- loadBMP "Images/rampaSTerra.bmp"
    rampaSRelva <- loadBMP "Images/rampaSRelva.bmp"
    rampaSLama  <- loadBMP "Images/rampaSLama.bmp"
    rampaSCola  <- loadBMP "Images/rampaSCola.bmp"
    rampaSBoost <- loadBMP "Images/rampaSBoost.bmp"
    rampaDRelva <- loadBMP "Images/rampaDRelva.bmp"
    rampaDTerra <- loadBMP "Images/rampaDTerra.bmp"
    rampaDBoost <- loadBMP "Images/rampaDBoost.bmp"
    rampaDCola  <- loadBMP "Images/rampaDCola.bmp"
    rampaDLama  <- loadBMP "Images/rampaDLama.bmp"
    retaTerraAlt1  <- loadBMP "Images/retaTerraAlt1.bmp"
    retaRelvaAlt1  <- loadBMP "Images/retaRelvaAlt1.bmp"
    retaLamaAlt1   <- loadBMP "Images/retaLamaAlt1.bmp"
    retaBoostAlt1  <- loadBMP "Images/retaBoostAlt1.bmp"
    retaColaAlt1   <- loadBMP "Images/retaColaAlt1.bmp"
    plateia        <- loadBMP "Images/plateia.bmp"



    play dm                       -- janela onde irá correr o jogo
           (green)                -- côr do fundo da janela
           fr                        -- frame rate
           (estadoGlossInicial [FiguraJog marioK 3 0 0,
                                FiguraPec retaRelva (Recta Relva 0),
                                FiguraPec retaTerra (Recta Terra 0), 
                                FiguraPec retaLama (Recta Lama 0),
                                FiguraPec retaCola (Recta Cola 0),
                                FiguraPec retaBoost (Recta Boost 0),
                                FiguraPec rampaSTerra (Rampa Terra 0 1), 
                                FiguraPec rampaSRelva (Rampa Relva 0 1), 
                                FiguraPec rampaSLama (Rampa Lama 0 1),
                                FiguraPec rampaSCola (Rampa Cola 0 1), 
                                FiguraPec rampaSBoost (Rampa Boost 0 1),
                                FiguraPec rampaDTerra (Rampa Terra 1 0), 
                                FiguraPec rampaDRelva (Rampa Relva 1 0), 
                                FiguraPec rampaDLama (Rampa Lama 1 0),
                                FiguraPec rampaDCola (Rampa Cola 1 0), 
                                FiguraPec rampaDBoost (Rampa Boost 1 0),
                                FiguraPec retaRelvaAlt1 (Recta Relva 1),
                                FiguraPec retaTerraAlt1 (Recta Terra 1), 
                                FiguraPec retaLamaAlt1 (Recta Lama 1),
                                FiguraPec retaColaAlt1 (Recta Cola 1),
                                FiguraPec retaBoostAlt1 (Recta Boost 1),
                                FiguraFundo plateia
                               ])  -- estado inicial
        
           desenhaEstadoGloss        -- desenha o estado do jogo
           reageEventoGloss          -- reage a um evento
           reageTempoGloss           -- reage ao passar do tempo
           
-- | Mapa que está representado graficamente
m :: Mapa 
m = [[Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Relva 0,Recta Lama 0],
     [Recta Terra 0,Recta Relva 0,Recta Boost 0,Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Lama 0,Recta Lama 0],
     [Recta Terra 0,Recta Terra 0,Recta Relva 0,Rampa Terra 0 1,Rampa Lama 1 0,Rampa Relva 0 1,Rampa Lama 1 0,Recta Lama 0],
     [Recta Terra 0,Recta Lama 0,Recta Relva 0,Recta Terra 0,Recta Boost 0,Rampa Boost 0 1,Recta Terra 1,Rampa Terra 1 0],
     [Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Boost 0,Rampa Terra 0 1,Recta Boost 1,Recta Cola 1,Rampa Terra 1 0],
     [Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Rampa Lama 0 1,Rampa Lama 1 0,Recta Lama 0],
     [Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0],
     [Recta Terra 0,Recta Relva 0,Recta Cola 0,Recta Boost 0,Recta Relva 0,Recta Lama 0,Recta Terra 0,Recta Relva 0]]



