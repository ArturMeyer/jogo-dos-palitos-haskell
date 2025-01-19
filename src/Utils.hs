-- Artur Welerson Sott Meyer - 202065552C

module Utils (
    pedirNumero,
    modificarFileira,
    imprimirPalitinhos,
    gerarListaAleatoriaState,
    calculaXorSom,
    escolherDificuldade,
    GameState,
    Escolha(..)
) where

import Control.Monad.State
import System.Random
import Text.Read (readMaybe)
import Data.Bits (xor)

data Escolha = Facil | Dificil | MvsM | Dificil_Ajuda | Sair deriving (Show, Read, Eq)
type GameState = ([Int], Escolha)

-- Valida a entrada de um número com base em um predicado
pedirNumero :: String -> (Int -> Bool) -> IO Int
pedirNumero prompt validacao = do
    putStrLn prompt
    numStr <- getLine
    case readMaybe numStr :: Maybe Int of
        Just n | validacao n -> return n
        _ -> do
            putStrLn "Entrada inválida. Tente novamente."
            pedirNumero prompt validacao

-- Modifica uma fileira específica com base no índice e na quantidade
modificarFileira :: [Int] -> Int -> Int -> [Int]
modificarFileira filas i n =
    let (antes, fileira:depois) = splitAt i filas
    in antes ++ [max 0 (fileira - n)] ++ depois

-- Imprime os palitos em forma visual
imprimirPalitinhos :: [Int] -> IO ()
imprimirPalitinhos filas =
    mapM_ (\(i, n) -> putStrLn (show i ++ ": " ++ replicate n '|')) (zip [0..] filas)

-- Gera uma lista aleatória de números (representando as filas de palitos)
gerarListaAleatoriaState :: State StdGen [Int]
gerarListaAleatoriaState = do
    g <- get
    let (num, g') = randomR (2, 1000) g
    let lista = take num $ filter odd (randomRs (1, 7) g')
    put g'
    return lista

-- Calcula a soma XOR da lista
calculaXorSom :: [Int] -> Int
calculaXorSom = foldl xor 0

escolherDificuldade :: IO Escolha
escolherDificuldade = do
    putStrLn "Escolha a dificuldade (Facil/Dificil/Dificil_Ajuda/MvsM/Sair):"
    dificuldade <- getLine  
    case readMaybe dificuldade :: Maybe Escolha of
        Just d  -> return d 
        Nothing -> do
            putStrLn "Entrada inválida. Tente novamente." 
            escolherDificuldade 