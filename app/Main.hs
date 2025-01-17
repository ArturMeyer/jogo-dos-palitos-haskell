-- Artur Welerson Sott Meyer - 202065552C

import GameLogic  -- Importa os construtores de Escolha e as funções de jogo
import Utils

import System.Random (newStdGen)
import Control.Monad.State

-- Função principal
main :: IO ()
main = do
    putStrLn "Jogo dos Palitos"
    escolha <- escolherDificuldade -- Pede para o usuário escolher a dificuldade
    g <- newStdGen  -- Pega o gerador de números aleatórios
    let estadoInicial = evalState (inicializarJogo escolha) g  -- Inicializa o estado do jogo
    case escolha of
        Facil -> do
            print estadoInicial  -- Mostra o estado inicial do jogo
            imprimirPalitinhos (fst estadoInicial)
            putStrLn "Iniciando o Jogo dos Palitos - Fácil"
            evalStateT jogoFacil estadoInicial  -- Executa o jogo fácil
            main
        Dificil -> do 
            print estadoInicial  -- Mostra o estado inicial do jogo
            imprimirPalitinhos (fst estadoInicial)
            putStrLn "Iniciando o Jogo dos Palitos - Difícil"
            evalStateT jogoDificil estadoInicial  -- Executa o jogo difícil
            main
        Dificil_Ajuda -> do 
            print estadoInicial  -- Mostra o estado inicial do jogo
            imprimirPalitinhos (fst estadoInicial)
            putStrLn "Iniciando o Jogo dos Palitos - Difícil_Ajuda"
            evalStateT jogoDificil estadoInicial  -- Executa o jogo difícil
            main
        MvsM -> do
            print estadoInicial  -- Mostra o estado inicial do jogo
            imprimirPalitinhos (fst estadoInicial)
            putStrLn "Iniciando o Jogo dos Palitos - Duelo de máquinas"
            evalStateT jogoMaquinaContraMaquina estadoInicial  -- Executa o jogo difícil
            main
        Sair -> do 
            putStrLn "Até mais!"
            putStrLn "Jogo encerrado"
            return ()
