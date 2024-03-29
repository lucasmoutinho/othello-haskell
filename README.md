# othello-haskell
An onthello game implemented in haskell with MinMax

## Executando
Existem dois modos de jogo: 1P vs PC (othello.hs) e 1P vs 2P (othello_2P.hs)

Para jogar execute a seguinte command line no terminal:
```
ghci othello || ghci othello_2P
```

No interpretador execute o seguinte comando:
```
main
```

## Informações adicionais

Reversi (othello) é um jogo de dois jogadores em um tabuleiro de 8x8. O sesguinte programa executa uma inteligencia artificial (implementada com minMax) para jogar contra um jogador. Utiliza-se uma estratégia que analisa o número de peças e número de posições para um score da vantagem em relação ao outro jogador (ambas informações são imprimidas juntamente ao tabuleiro a cada rodada). Foi utilizado o símbolo “X” (Black - Preto) para o computador e “O” (White - Branco) para o jogador.
A cada rodada o programa:
1. Pede ao jogador as coordenadas de onde colocar a peça
2. Reemprime o tabuleiro com a peça posicionada e, quando necessário, as peças viradas.
3. Pede que o jogador aperte enter
4. Imprime o tabuleiro após a jogada do computador, indicado as coordenadas escolhidas;

Observação: O mixmax está realizando a recursão em 3 níveis da árvore. Portanto, dependendo da jogada, o computador pode passar mais de um minuto para dar uma resposta. Para fins didáticos a nossa IA tem o seguinte rosto (0.0)

### Developers
- Lucas Moutinho