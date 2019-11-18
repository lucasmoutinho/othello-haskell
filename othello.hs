import Data.List
import Data.Maybe
import System.IO
import qualified Data.Map as Map -- Importa a estrutura de dados Map que consiste de (key, value)

data Piece = Empty | Black | White deriving(Eq, Show)
type Position = (Int,Int)
type Cell = (Position, Piece)
type Board = Map.Map Position Piece
type Direction = Position
type Movement = (Position,Direction)
type ValidMovement = [Bool] -- para cada uma das 8 direções [(1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1), (0,-1), (1,-1)]

possibleDirections :: [(Direction)]
possibleDirections = [(1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1), (0,-1), (1,-1)]

-- Cria tabuleiro com peças vazias
createTable :: Piece -> [Cell]
createTable z = [((x,y),z) | x <- [0..7] , y <- [0..7]]

-- Inicia o tabuleiro com as peças iniciais
-- O Map.union prioriza os valores da key da esquerda
initializeBoard :: Board
initializeBoard = Map.union (Map.fromList [((3, 3), White), ((4, 4), White), ((3, 4), Black), ((4, 3), Black)]) (Map.fromList (createTable Empty))

-- Retorna a cor oposta do jogador da vez
oppositeColor :: Piece -> Piece
oppositeColor piece =
    case piece of
        White -> Black
        Black -> White 
        Empty -> Empty

-- Checa se a posição escolhida é valida
isValidPosition :: Position -> Bool
isValidPosition (x,y) = if x < 0 || x > 7 || y < 0 || y > 7 then
                            False
                        else
                            True

-- Soma duas chaves de posição
addDirection :: Movement -> Position
addDirection ((a,b),(x,y)) = (a+x, b+y)

-- Checa se na direção existe um movimento válido
isValidDirection :: Movement -> Piece -> Board -> Bool
isValidDirection (move_position, direction) color board = if isValidPosition (addDirection (move_position, direction)) then do
                                                            let new_move = addDirection (move_position, direction)
                                                                value = (board Map.! new_move)
                                                            if value == color then
                                                                True
                                                            else
                                                                if value == Empty then
                                                                    False
                                                                else
                                                                    isValidDirection (new_move, direction) color board
                                                        else False

-- Checa se tem a possibilidade de movimento naquela direção
isPossibleDirection :: Movement -> Piece -> Board -> Bool
isPossibleDirection movement color board = if isValidPosition (addDirection movement) then do
                                            let value = (board Map.! (addDirection movement))
                                            if value == color then
                                                False
                                            else
                                                True
                                        else
                                            False

-- Checa se existe algum movimento valido na posicao escolhida pelo jogador e retorna um array de booleanos informando se o movimento é valido em cada direção
isValidMovement :: Position -> Piece -> Board -> ValidMovement
isValidMovement position color board = map (\move -> if (isPossibleDirection move color board) then (isValidDirection move color board) else False ) (map (\possible_direction -> (position,possible_direction)) possibleDirections)

-- Array de movimento inválido
invalidMove :: ValidMovement
invalidMove = take 8 (cycle [False])

-- Retorna a posição caso exista algum movimento possível nela
isAvailablePosition :: Position -> Piece -> Board -> [Position]
isAvailablePosition position color board = if (board Map.! position) == Empty then do
                                                if ((isValidMovement position color board) == invalidMove) then
                                                    []
                                                else
                                                    [position]
                                            else
                                                []

-- Retorna uma lista com posições para movimentos válidos
availablePositions :: Piece -> Board -> [Position]
availablePositions color board = concat (map (\position -> (isAvailablePosition position color board)) (Map.keys board) )

-- Troca uma posição vazia por uma peça
setColor :: Position -> Piece -> Board -> Board 
setColor pos color board = if board Map.! pos == Empty then
                        Map.union (Map.fromList [(pos,color)]) (board)
                    else
                        board

-- Inverte as cores do adversario
changePieces :: Position -> Direction -> Piece -> Board -> Board 
changePieces pos dir color board = if isValidPosition (addDirection (pos,dir)) then do
                                        let new_move = (addDirection (pos,dir))
                                            value = (board Map.! new_move)
                                        if value == color then
                                            board
                                        else
                                            let newboard = (Map.union (Map.fromList [(new_move,color)]) (board)) -- muda a cor
                                            in changePieces new_move dir color newboard
                                    else board

-- Troca as peças do tabuleiro. Troca uma posição vazia por uma peça preta e inverte as peças do adversario nas direções corretas
setAndChangePieces :: Position -> [Direction] -> Piece -> Board -> Board
setAndChangePieces pos possible_direction color board = if possible_direction /= [] then
                                                            let newboard = (changePieces pos (head possible_direction) color board)
                                                            in setAndChangePieces pos (tail possible_direction) color newboard
                                                        else
                                                            (setColor pos color board)

-- cria uma lista com todas as direções em que o movimento feito irá inverter as peças do adversario
createAvailableDirection :: Position -> Piece -> Board -> [Direction]
createAvailableDirection pos color board = (concat (zipWith (\i valid -> if valid then [possibleDirections!!i] else []) [0..] (isValidMovement pos color board)))

-- Realiza uma jogada. Procura direções em que a jogada terá efeito e chama a função que altera as peças do tabuleiro
makeMove :: Position -> Piece -> Board -> Board
makeMove pos color board = let possible_direction = createAvailableDirection pos color board
                            in setAndChangePieces pos possible_direction color board

-- Verifica se a escolha é válida
isLegalChoice :: Position -> [Position] -> Bool
isLegalChoice pos possible_positions = (pos `elem` possible_positions)

-- Numero de pecas que um jogador possui a mais que o outro
piecesScore :: Piece -> Board -> Int
piecesScore color board = sum (map (\(_, value) -> scorePerPiece value) (Map.toList board))
                        where 
                            scorePerPiece x
                                | (x == color) = 1
                                | (x == Empty) = 0
                                | otherwise = -1

-- Numero de posicoes que um jogador possui a mais que o outro
positionsScore :: Piece -> Board -> Int
positionsScore color board = scorePerPosition color board - scorePerPosition (oppositeColor color) board
                                where
                                    scorePerPosition color board = length $ availablePositions color board

-- Valor numerico que indica a vantagem de um jogador para o outro. Utilizado na lógica do minmax
advantageScore :: Piece -> Board -> Int
advantageScore color board = piecesScore color board + 10 * positionsScore color board

-- Funcao que calcula o minmax para uma posicao. Recebe um valor (depth) de quantas posicoes a frente do tabuleiro ira enxergar.
minMax :: Int -> Piece -> Board -> Int
minMax depth color board
    | gameOver = if piecesScore color board > 0 then 1000000 else -1000000 -- Se a partida acaba com o pc ganhando, retorna um valor imenso
    | depth <= 0 = advantageScore color board -- chegou no final da árvore, retorna o valor recursivamente
    | otherwise = if nextColor /= color then (- maxAdvantageForNextColor) else (maxAdvantageForNextColor) -- recursivamente encontra o mixmax pela arvore
    where
        opponentMoves = availablePositions (oppositeColor color) board
        playerMoves = availablePositions color board
        gameOver = playerMoves == [] && opponentMoves == []
        nextColor = if opponentMoves /= [] then oppositeColor color else color
        movesForNextColor = if nextColor /= color then opponentMoves else playerMoves
        maxAdvantageForNextColor = maximum (map (\position -> minMax (depth-1) nextColor (makeMove position nextColor board)) movesForNextColor)

-- Retorna a melhor posição de acordo com a função de mixmax. Calcula-se o minmax para cada posição do vetor de posições disponíveis
bestPosition :: (Piece -> Board -> Int) -> Piece -> Board -> Position
bestPosition minmaxfunction color board = (\(key, _) -> key) (maximumBy (\(_, value_a) (_, value_b) -> compare value_a value_b) (map (\position -> (position, minmaxfunction color (makeMove position color board))) (availablePositions color board)))

-- Imprime a string correta para cada peça
printPiece :: Piece -> [Char] 
printPiece piece =
    case piece of
        Empty -> " "
        White -> "O"
        Black -> "X"

-- Imprime uma linha do tabuleiro
printRow :: Int -> Board -> [Char]
printRow row_number board = show row_number ++ " |" ++ (intercalate "|" (map (\pos -> printPiece (board Map.! pos)) ([(row_number,x) | x <- [0..7]]))) ++ "|"

-- Imprime o tabuleiro do jogo (deve ser passado para um putStr)
printBoard :: Board -> [Char]
printBoard board = "   " ++ (intercalate " " (map (\x -> show x) [0..7])) ++ "\n" ++ (intercalate "\n" (map (\y -> printRow y board) [0..7])) ++ "\n\n"

-- Imprime as posições disponíveis para a próxima jogada
printAvailablePositions :: Piece -> Board -> [Char]
printAvailablePositions color board = "Possiveis movimentos para a cor " ++ show color ++ ": " ++ (intercalate " " (map (\y -> show y) (availablePositions color board))) ++ "\n"

-- Imprime corretamente o score da partida
printCurrentScore :: Board -> [Char]
printCurrentScore board = "Vantagem de peças atual -- Brancas: " ++ (show (piecesScore White board)) ++ " -- Pretas: " ++ (show (piecesScore Black board)) ++ "\nVantagem de posicoes atual -- Brancas: "  ++ (show (positionsScore White board)) ++ " -- Pretas: " ++ (show (positionsScore Black board)) ++ "\n\n"

-- Aguarde pelo usuario pressionar enter
ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
    where 
        f True = x >>= return . Just
        f _ = return Nothing

userMovement color board = do 
    putStr "\n"
    let endGame = availablePositions color board == [] && availablePositions (oppositeColor color) board == []
    if endGame then do
        let printable_board = printBoard board
        putStr printable_board
        let score_print = printCurrentScore board
        putStr score_print
        putStr "O jogo acabou, nenhum movimento possível para ambos os lados\n"
        if (piecesScore White board > 0) then do
            putStr "Parabéns, você ganhou\n"
        else do
            if (piecesScore White board == 0) then do
                putStr "Empate!\n"
            else do
                putStr "Voce perdeu, 0.0\n"
    else do
        putStr ("Jogador do turno: " ++ show color)
        if color == White then putStr("-- Sua vez de jogar\n\n") else putStr("-- Vez do computador\n\n")
        let printable_board = printBoard board
        putStr printable_board
        let score_print = printCurrentScore board
        putStr score_print
        let available_pos_print = printAvailablePositions color board
        putStr available_pos_print
        let available_positions = availablePositions color board
        if color == White then do
            -- Opcoes do jogador
            if available_positions == [] then do
                putStr "Nenhum movimento possivel, perca a vez\nPressione enter para continuar...\n"
                hSetBuffering stdin NoBuffering
                x <- getChar
                putStrLn ("Enter" ++ [x])
                userMovement (oppositeColor color) board
            else do
                putStr "Escolha uma coordenada (x,y): "
                position <- readLn
                if (isLegalChoice position available_positions) then do
                    let new_board = makeMove position color board
                    userMovement (oppositeColor color) new_board
                else do
                    putStr "Posicao invalida\n"
                    userMovement color board
        else do
            -- Opcoes do computador
            putStr ("Computador está escolhendo a jogada... 0.0\n")
            let best_choice = bestPosition (minMax 3) color board
            let new_board = makeMove best_choice color board
            putStr ("Coordenada (x,y) escolhida pelo computador: " ++ show best_choice ++ "   :) tee hee" ++ "\nPressione enter para continuar...\n")
            hSetBuffering stdin NoBuffering
            x <- getChar
            putStrLn ("Enter" ++ [x])
            userMovement (oppositeColor color) new_board

main = do 
        putStr "\n\n\n--- Bem vindo ao jogo Othello! ---\nPeças brancas comecam\n\n\n"
        userMovement White initializeBoard