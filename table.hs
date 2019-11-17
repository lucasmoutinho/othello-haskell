import Data.List
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

-- Checa se existe algum movimento valido na posicao escolhida pelo jogador
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

-- Troca uma posição vazia por uma peça preta
setBlack :: Position -> Board -> Board 
setBlack pos board = if board Map.! pos == Empty then
                        Map.union (Map.fromList [(pos,Black)]) (board)
                    else
                        board

-- Troca uma posição vazia por uma peça branca
setWhite :: Position -> Board -> Board 
setWhite pos board = if board Map.! pos == Empty then
                        Map.union (Map.fromList [(pos,White)]) (board)
                    else
                        board

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

printAvailablePositions:: Piece -> Board -> [Char]
printAvailablePositions color board = "Possíveis movimentos para a cor " ++ show color ++ ": " ++ (intercalate " " (map (\y -> show y) (availablePositions color board))) ++ "\n"