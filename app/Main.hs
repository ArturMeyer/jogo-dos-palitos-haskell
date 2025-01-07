import Lib

main :: IO ()
main = do
    tabuleiro <- gerarTabuleiro 1 10
    putStrLn $ "Tabuleiro gerado: " ++ show tabuleiro

    escolha <- escolheModoDeJogo
    if escolha == "F" then putStrLn $ "Modo Fácil" else putStrLn $ "Modo Difícil"

    