module Lib
    ( 
        getPalitosFileiraI,
        gerarTabuleiro,
        escolheModoDeJogo
    ) where
    
import System.Random (randomRIO)

data Tabuleiro = Tabuleiro [Int] deriving (Show)

gerarTabuleiro :: Int -> Int -> IO Tabuleiro
gerarTabuleiro minF maxF = do
    randomNumber <- randomRIO (minF, maxF)
    valores <- mapM (\_ -> randomRIO (1, 7)) [1 .. randomNumber]
    return (Tabuleiro valores)

getPalitosFileiraI :: Tabuleiro -> Int -> Int
getPalitosFileiraI (Tabuleiro fileira) i =  fileira !! i

escolheModoDeJogo :: IO String
escolheModoDeJogo = do
    putStrLn "Escolha o modo de jogo (F para Fácil, D para Difícil):"
    modo <- getLine
    return modo

    

