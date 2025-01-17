-- Artur Welerson Sott Meyer - 202065552C

module GameLogic (
    inicializarJogo,
    maquinaFacil,
    maquinaDificil,
    jogoFacil,
    jogoDificil,
    jogoMaquinaContraMaquina
) where

import Control.Monad.State
import System.Random
import Utils 
import Data.Bits (xor)

-- Inicializa o jogo com dificuldade e lista de palitos
inicializarJogo :: Escolha -> State StdGen GameState
inicializarJogo dificuldade = do
    lista <- gerarListaAleatoriaState
    return (lista, dificuldade)

-- Jogada fácil da máquina
maquinaFacil :: StateT GameState IO ()
maquinaFacil = do
    (filas, dificuldade) <- get
    let listaSemZeros = filter (\(_, v) -> v /= 0) (zip [0..] filas)
    maq_i <- liftIO $ randomRIO (0, length listaSemZeros - 1)
    maq_n <- liftIO $ randomRIO (1, snd (listaSemZeros !! maq_i))
    let filasAtualizadas = modificarFileira filas (fst (listaSemZeros !! maq_i)) maq_n
    put (filasAtualizadas, dificuldade)
    liftIO $ putStrLn $ "Máquina removeu " ++ show maq_n ++ " palitos da fileira " ++ show (fst (listaSemZeros !! maq_i))

-- Jogada difícil da máquina
maquinaDificil :: StateT GameState IO ()
maquinaDificil = do
    (filas, dificuldade) <- get
    let somXor = calculaXorSom filas
    if somXor /= 0 then do
        let (maq_i, maq_n) = head [(i, filas !! i - (filas !! i `xor` somXor)) | i <- [0..length filas - 1], (filas !! i `xor` somXor) < filas !! i]
        let novasFilas = modificarFileira filas maq_i maq_n
        put (novasFilas, dificuldade)
        liftIO $ putStrLn $ "Máquina fez jogada estratégica: removeu " ++ show maq_n ++ " palitos da fileira " ++ show maq_i
        if(dificuldade == Dificil_Ajuda) then do
            liftIO $ putStrLn $ "Nim_sum = " ++ show (calculaXorSom novasFilas)
            else 
                liftIO $ putStrLn $ "--"
    else do
        maquinaFacil

-- Jogo na dificuldade fácil
jogoFacil :: StateT GameState IO ()
jogoFacil = do
    (filas, dificuldade) <- get
    liftIO $ imprimirPalitinhos filas
    i <- liftIO $ pedirNumero "Escolha a fileira:" (\n -> (n >= 0) && (n < length filas) && ((filas !! n) /= 0))
    n <- liftIO $ pedirNumero "Quantos palitos deseja remover?" (\n -> n > 0 && n <= filas !! i)
    let filasAtualizadas = modificarFileira filas i n
    put (filasAtualizadas, dificuldade)
    if all (== 0) filasAtualizadas
        then liftIO $ putStrLn "Parabéns, você ganhou!"
        else do
            maquinaFacil
            (_, _) <- get
            if all (== 0) filasAtualizadas
                then liftIO $ putStrLn "Você perdeu :("
                else jogoFacil

-- Jogo na dificuldade difícil
jogoDificil :: StateT GameState IO ()
jogoDificil = do
    maquinaDificil
    (filas, dificuldade) <- get 
    if all (== 0) filas
        then liftIO $ putStrLn "Você perdeu :("
    else do
        liftIO $ imprimirPalitinhos filas
        i <- liftIO $ pedirNumero "Escolha a fileira:" (\n -> n >= 0 && n < length filas && ((filas !! n) /= 0))
        n <- liftIO $ pedirNumero "Quantos palitos deseja remover?" (\n -> n > 0 && n <= filas !! i )
        let filasAtualizadas = modificarFileira filas i n
        put (filasAtualizadas, dificuldade)
        if all (== 0) filasAtualizadas
            then liftIO $ putStrLn "Parabéns, você ganhou!"
            else do
                (_, _) <- get
                jogoDificil

-- Jogo com duas máquinas (uma fácil e outra difícil)
jogoMaquinaContraMaquina :: StateT GameState IO ()
jogoMaquinaContraMaquina = do
    (filas, dificuldade) <- get
    liftIO $ imprimirPalitinhos filas
    if all (== 0) filas
        then liftIO $ putStrLn "O jogo terminou!"
        else do
            -- Turno da máquina fácil
            liftIO $ putStrLn "\nTurno da Máquina Fácil:"
            maquinaFacil
            (novasFilas1, _) <- get
            if all (== 0) novasFilas1
                then liftIO $ putStrLn "Máquina Fácil venceu!"
                else do
                    -- Turno da máquina difícil
                    liftIO $ putStrLn "\nTurno da Máquina Difícil:"
                    maquinaDificil
                    (novasFilas2, _) <- get
                    if all (== 0) novasFilas2
                        then liftIO $ putStrLn "Máquina Difícil venceu!"
                        else jogoMaquinaContraMaquina
