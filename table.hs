import Data.List
import qualified Data.Map as Map -- Importa a estrutura de dados Map que consiste de (key, value)

data Piece = Empty | Black | White deriving(Eq, Show)
type Position = (Int,Int)
type Cell = (Position, Piece)
type Board = Map.Map Position Piece

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
isValidPosition (x,y) = if x < 0 || x > 7 then
                            False
                        else
                            True

-- Soma duas chaves de posição
addDirection :: Position -> Position -> Position
addDirection (a,b) (x,y) = (a+x, b+y)

-- Checa se na direção existe um movimento válido
isValidDirection :: Position -> Position -> Piece -> Board -> Bool
isValidDirection move_position direction color board = if isValidPosition move_position then do
                                                        let new_direction = (addDirection move_position direction)
                                                            value = (board Map.! move_position)
                                                        if value == color then
                                                            True
                                                        else
                                                            if value == Empty then
                                                                False
                                                            else
                                                                isValidDirection (new_direction) (direction) (color) (board)
                                                    else 
                                                        False

-- isValidMovement :: 

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
printBoard board = "   " ++ (intercalate " " (map (\x -> show x) [0..7])) ++ "\n" ++ (intercalate "\n" (map (\y -> printRow y board) [0..7])) ++ "\n"