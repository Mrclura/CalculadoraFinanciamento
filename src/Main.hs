module Main where

import Data.List (foldl')
import Control.Monad.State
import System.IO (hFlush, stdout)
import Text.Printf (printf)

type Historico = [Simulacao]

data Simulacao = Simulacao
  { valorObjeto        :: Double
  , entrada            :: Double
  , parcelas           :: Int
  , taxaMensal         :: Double
  , valorFinanciado    :: Double
  , parcelaMensal      :: Double
  , custoEfetivoTotal  :: Double
  , valorTotalPago     :: Double
  } deriving Show

-- Função para calcular o valor da parcela fixa
calcularParcelaFixa :: Double -> Double -> Int -> Double
calcularParcelaFixa valorFinanciado taxaMensal numParcelas =
  let taxa = taxaMensal / 100
  in (valorFinanciado * taxa * (1 + taxa) ^ fromIntegral numParcelas) / ((1 + taxa) ^ fromIntegral numParcelas - 1)

-- Função para calcular o valor total pago e o custo efetivo total, utilizando foldl
calcularCETeTotalPago :: Double -> Double -> Int -> (Double, Double)
calcularCETeTotalPago parcela valorFinanciado numParcelas =
  let parcelasList = replicate numParcelas parcela
      valorTotalPago = foldl' (+) 0 parcelasList
      custoEfetivoTotal = ((valorTotalPago - valorFinanciado) / valorFinanciado) * 100
  in (valorTotalPago, custoEfetivoTotal)

-- Função para gerar a tabela de amortização de forma recursiva
gerarTabelaAmortizacao :: Double -> Double -> Int -> Double -> [(Int, Double, Double, Double, Double)]
gerarTabelaAmortizacao valorFinanciado taxaMensal numParcelas parcela = gerarParcelas valorFinanciado 1
  where
    taxa = taxaMensal / 100
    
    gerarParcelas :: Double -> Int -> [(Int, Double, Double, Double, Double)]
    gerarParcelas saldoDevedor i
      | i > numParcelas = []
      | otherwise =
          let juros = saldoDevedor * taxa
              amortizacao = parcela - juros
              saldoDevedorNovo = saldoDevedor - amortizacao
          in (i, parcela, juros, amortizacao, saldoDevedorNovo) : gerarParcelas saldoDevedorNovo (i + 1)

-- Adicionar simulação ao histórico
adicionarSimulacao :: Double -> Double -> Int -> Double -> State Historico Simulacao
adicionarSimulacao valorObjeto entrada numParcelas taxaMensal = do
  let valorFinanciado = valorObjeto - entrada
      parcela = calcularParcelaFixa valorFinanciado taxaMensal numParcelas
      (valorTotalPago, custoEfetivoTotal) = calcularCETeTotalPago parcela valorFinanciado numParcelas
      simulacao = Simulacao valorObjeto entrada numParcelas taxaMensal valorFinanciado parcela custoEfetivoTotal valorTotalPago
  historico <- get
  put (simulacao : historico)
  return simulacao

-- Mostrar histórico de simulações
mostrarHistorico :: Historico -> String
mostrarHistorico [] = "Nenhuma simulação registrada."
mostrarHistorico historico = unlines $ map show historico

-- Formatar números com duas casas decimais
formatarDouble :: Double -> String
formatarDouble = printf "%.2f"

main :: IO ()
main = do
  let loop historico = do
        putStrLn "Digite o valor do objeto financiado: "
        hFlush stdout
        valorObjeto <- readLn
        putStrLn "Digite o valor da entrada: "
        hFlush stdout
        entrada <- readLn
        putStrLn "Digite o número total de parcelas: "
        hFlush stdout
        parcelas <- readLn
        putStrLn "Digite a taxa de juros mensal (em porcentagem, por exemplo, 1 para 1%): "
        hFlush stdout
        taxa <- readLn
        let simulacao = evalState (adicionarSimulacao valorObjeto entrada parcelas taxa) historico
        let parcela = calcularParcelaFixa (valorObjeto - entrada) taxa parcelas
        let tabela = gerarTabelaAmortizacao (valorObjeto - entrada) taxa parcelas parcela
        putStrLn $ "Valor financiado: " ++ formatarDouble (valorFinanciado simulacao)
        putStrLn $ "Parcela mensal: " ++ formatarDouble parcela
        putStrLn $ "Custo efetivo total (CET): " ++ formatarDouble (custoEfetivoTotal simulacao) ++ "%"
        putStrLn $ "Valor total pago: " ++ formatarDouble (valorTotalPago simulacao)
        putStrLn "Tabela de amortização:"
        putStrLn "Parcela\tValor\tJuros\tAmortização\tSaldo Devedor"
        mapM_ (\(parcelaNum, valor, juros, amortizacao, saldoDevedor) ->
                 putStrLn $ printf "%d\t%s\t%s\t%s\t%s"
                   parcelaNum (formatarDouble valor) (formatarDouble juros) (formatarDouble amortizacao) (formatarDouble saldoDevedor)) tabela
        putStrLn "Histórico de simulações: "
        putStrLn $ mostrarHistorico (simulacao : historico)
        putStrLn "Deseja adicionar mais uma simulação? (s/n): "
        hFlush stdout
        resposta <- getLine
        case resposta of
          "s" -> loop (simulacao : historico)
          _   -> putStrLn "Programa encerrado."
  loop []
