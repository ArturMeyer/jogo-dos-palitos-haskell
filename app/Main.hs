import System.Random
import Control.Monad
import Control.Monad.State
import Text.Read (readMaybe)

-- Tipos e Definições
data Escolha = Facil | Dificil | Sair deriving (Show, Read)

type GameState = ([Int], Escolha)

-- Gera uma lista aleatória de números (representando as filas de palitos)
gerarListaAleatoriaState :: State StdGen [Int]
gerarListaAleatoriaState = do
    g <- get  -- Pega o gerador de números aleatórios do estado
    let (num, g') = randomR (2, 10) g       -- Número de elementos da lista
    let lista = take num $ randomRs (1, 7) g'  -- Gera os palitos para a lista
    put g'  -- Atualiza o estado com o novo gerador
    return lista  -- Retorna a lista gerada

-- Inicializa o jogo com a dificuldade e a lista de palitos
inicializarJogo :: Escolha -> State StdGen GameState
inicializarJogo dificuldade = do
    lista <- gerarListaAleatoriaState  -- Gera a lista de palitos
    return (lista, dificuldade)  -- Retorna o estado inicial

-- Função para escolher a dificuldade do jogo
escolherDificuldade :: IO Escolha
escolherDificuldade = do
    putStrLn "Escolha a dificuldade (Facil/Dificil/Sair):"
    dificuldade <- getLine  -- Lê a escolha do usuário
    case readMaybe dificuldade :: Maybe Escolha of
        Just d  -> return d  -- Se a conversão for bem-sucedida, retorna a dificuldade
        Nothing -> do
            putStrLn "Entrada inválida. Tente novamente."  -- Se a conversão falhar, solicita nova entrada
            escolherDificuldade  -- Repete o processo

-- Função para pedir um número ao usuário e validar a entrada
pedirNumero :: String -> (Int -> Bool) -> StateT GameState IO Int
pedirNumero prompt validacao = do
    liftIO $ putStrLn prompt
    numStr <- liftIO getLine
    case readMaybe numStr :: Maybe Int of
        Just n | validacao n -> return n  -- Se válido, retorna o número
        _ -> do
            liftIO $ putStrLn "Entrada inválida. Tente novamente."
            pedirNumero prompt validacao  -- Repete até o usuário fornecer uma entrada válida

-- Função para modificar a fileira de palitos
modificarFileira :: [Int] -> Int -> Int -> [Int]
modificarFileira filas i n =
    let (antes, fileira:depois) = splitAt i filas  -- Divide as filas antes e depois da fileira
    in antes ++ [max 0 (fileira - n)] ++ depois  -- Atualiza a fileira e garante que não seja negativo

-- Função para imprimir os palitinhos em forma visual
imprimirPalitinhos :: [Int] -> IO ()
imprimirPalitinhos filas = do
    let indexedList = zip [0..] filas  -- Adiciona o índice à lista de filas
    mapM_ imprimirPalito indexedList  -- Imprime os palitos para cada fila

-- Função auxiliar para imprimir o palito de cada fileira
imprimirPalito :: (Int, Int) -> IO ()
imprimirPalito (i, n) = putStrLn (show i ++ " - " ++ replicate n '|')  -- Imprime o índice e a quantidade de palitos


maquinaFacil :: StateT GameState IO()
maquinaFacil = do
    (filas, dificuldade) <- get
    let listaSemZeros = filter (\(_, v) -> v /= 0) (zip [0..] filas)
    maq_i <- randomRIO(0 :: Int, (length listaSemZeros)-1 :: Int)
    maq_n <- randomRIO(1 :: Int, (snd (listaSemZeros !! maq_i)) :: Int)
    liftIO $ putStrLn $ "Estado atual das filas: " ++ show filas
    liftIO $ putStrLn $ "Vez da Máquina"
    let filasAtualizadas = modificarFileira filas (fst (listaSemZeros !! maq_i)) maq_n
    put (filasAtualizadas, dificuldade)
    liftIO $ putStrLn $ "Maquina jogou em " ++ show (fst (listaSemZeros !! maq_i)) ++ " tirou " ++ show maq_n ++" e ela foi atualizada para: " ++ show (filasAtualizadas !! (fst (listaSemZeros !! maq_i)))

-- Função principal para o jogo fácil
jogoFacil :: StateT GameState IO ()
jogoFacil = do
    (filas, dificuldade) <- get

    liftIO $ putStrLn $ "Estado atual das filas: " ++ show filas
    
    -- Valida o índice da fileira (deve ser um número dentro do intervalo das filas)
    i <- pedirNumero "Qual fileira deseja mudar? (Digite um número)" (\n -> n >= 0)
    
    -- Valida a quantidade de palitos (deve ser maior que 0)
    n <- pedirNumero "Quantos palitos você deseja remover? (Digite um número)" (\n -> n > 0)
    
    -- Pega o estado atual
    
    -- Verifica se o índice é válido dentro das filas
    if i < length filas then do
        if n > (filas !! i)
            then do
                liftIO $ putStrLn "Número de palitos muito alto."
                jogoFacil  -- Continua o jogo
            else do
                -- Atualiza o estado
                let filasAtualizadas = modificarFileira filas i n
                put (filasAtualizadas, dificuldade)
                liftIO $ (putStrLn $ "Fileira " ++ show i ++ " foi atualizada para: " ++ show (filasAtualizadas !! i))
                (filas, dificuldade) <- get
                liftIO $ imprimirPalitinhos filas
                if (all (== 0) filas) 
                    then do
                         liftIO $ putStrLn $ "Parabéns você ganhou!"
                         return ()
                    else do 
                        maquinaFacil
                        (filas, dificuldade) <- get
                        liftIO $ imprimirPalitinhos filas
                        if (all (== 0) filas)  
                            then do 
                                liftIO $ putStrLn $ "Você perdeu :("
                                return ()
                            else jogoFacil
    else do
        liftIO $ putStrLn "Índice de fileira inválido."
        jogoFacil  -- Continua o jogo


-- Função de Jogo Difícil (sem lógica ainda)
jogoDificil :: StateT GameState IO ()
jogoDificil = do
    liftIO $ putStrLn "Iniciando o Jogo dos Palitos - Difícil"
    -- Aqui você pode adicionar a lógica do jogo difícil e manipulação de estado
    return ()

-- Função principal
main :: IO ()
main = do
    putStrLn "Jogo dos Palitos"
    escolha <- escolherDificuldade  -- Pede para o usuário escolher a dificuldade
    g <- getStdGen  -- Pega o gerador de números aleatórios
    let estadoInicial = evalState (inicializarJogo escolha) g  -- Inicializa o estado do jogo
    -- Executa o jogo com a dificuldade escolhida
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
        Sair -> do 
            putStrLn "Até mais!"
            putStrLn "Jogo encerrado"
            return ()
    


