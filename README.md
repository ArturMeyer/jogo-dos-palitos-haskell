# Jogo dos Palitos - Haskell

Este é um jogo dos palitos implementado em Haskell. O jogo segue o modelo tradicional de Nim, com diferentes níveis de dificuldade e opções para jogar contra a máquina ou contra outra máquina.

## Funcionalidades

- **Modo Fácil**: Jogue contra a máquina em um modo simples onde a máquina escolhe jogadas aleatórias.
- **Modo Difícil**: A máquina joga de maneira estratégica, utilizando a técnica do Nim-Sum para calcular a melhor jogada.
- **Modo Difícil com Ajuda**: A máquina ainda joga de maneira estratégica, mas exibe o valor do Nim-Sum para ajudar o jogador.
- **Modo MvsM**: Jogo entre duas máquinas, uma fácil e outra difícil.
- **Sair**: Encerra o jogo.

## Pré-requisitos

- [Stack](https://docs.haskellstack.org/en/stable/README/) para gerenciamento de dependências e construção do projeto.
- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler) para compilar e rodar o código.

## Como Rodar o Código

### 1. Clonar o repositório

Primeiro, clone o repositório para o seu ambiente local:

```bash
git clone https://github.com/ArturMeyer/jogo-dos-palitos-haskell.git
cd <DIRETORIO_DO_PROJETO>
