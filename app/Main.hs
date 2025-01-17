-- Artur Welerson Sott Meyer - 202065552C

import GameLogic
import Utils

import System.Random (newStdGen)
import Control.Monad.State

-- Função principal
main :: IO ()
main = do
    putStrLn "Jogo dos Palitos"
    escolha <- escolherDificuldade 
    g <- newStdGen
    let estadoInicial = evalState (inicializarJogo escolha) g  
    case escolha of
        Facil -> do
            print estadoInicial
            imprimirPalitinhos (fst estadoInicial)
            putStrLn "Iniciando o Jogo dos Palitos - Fácil"
            evalStateT jogoFacil estadoInicial 
            main
        Dificil -> do 
            print estadoInicial 
            imprimirPalitinhos (fst estadoInicial)
            putStrLn "Iniciando o Jogo dos Palitos - Difícil"
            evalStateT jogoDificil estadoInicial 
            main
        Dificil_Ajuda -> do 
            print estadoInicial 
            imprimirPalitinhos (fst estadoInicial)
            putStrLn "Iniciando o Jogo dos Palitos - Difícil_Ajuda"
            evalStateT jogoDificil estadoInicial 
            main
        MvsM -> do
            print estadoInicial
            imprimirPalitinhos (fst estadoInicial)
            putStrLn "Iniciando o Jogo dos Palitos - Duelo de máquinas"
            evalStateT jogoMaquinaContraMaquina estadoInicial 
            main
        Sair -> do 
            putStrLn "Até mais!"
            putStrLn "Jogo encerrado"
            return ()
