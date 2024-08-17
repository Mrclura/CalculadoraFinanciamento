module Main where

import Data.List (foldl')
import Control.Monad.State
import System.IO (hFlush, stdout)
import Text.Printf (printf)

type Historico = [Simulacao]
type ValorObjeto = Double
type ValorFinanciado = Double
type ValorEntrada = Double
type QntdParcelas = Int
type IdParcela = Int
type TaxaMensal = Double
type ValorTotalPago = Double
type CustoEfetivoTotal = Double
type ValorParcela = Double
type ValorAmortizado = Double
type JurosParcela = Double
type SaldoDevedor = Double

data Simulacao = Simulacao
  { valorObjeto        :: ValorObjeto
  , valorEntrada       :: ValorEntrada
  , qntdParcelas       :: Int
  , taxaMensal         :: TaxaMensal
  , valorFinanciado    :: ValorFinanciado
  , valorParcela       :: ValorParcela
  , custoEfetivoTotal  :: CustoEfetivoTotal
  , valorTotalPago     :: ValorTotalPago
  } deriving Show

-- Função para calcular o valor da parcela fixa
calcularParcelaFixa :: ValorFinanciado -> TaxaMensal -> QntdParcelas -> ValorParcela
calcularParcelaFixa valorFinanciado taxaMensal qntdParcelas =
  let taxa = taxaMensal / 100
  in (valorFinanciado * taxa * (1 + taxa) ^ fromIntegral qntdParcelas) / ((1 + taxa) ^ fromIntegral qntdParcelas - 1)

-- Função para calcular o valor total pago e o custo efetivo total, utilizando foldl
calcularCETeTotalPago :: ValorParcela -> ValorFinanciado -> QntdParcelas -> (ValorTotalPago, CustoEfetivoTotal)
calcularCETeTotalPago parcela valorFinanciado qntdParcelas =
  let qntdParcelasList = replicate qntdParcelas parcela
      valorTotalPago = foldl' (+) 0 qntdParcelasList
      custoEfetivoTotal = ((valorTotalPago - valorFinanciado) / valorFinanciado) * 100
  in (valorTotalPago, custoEfetivoTotal)

-- Função para gerar a tabela de amortização de forma recursiva
gerarTabelaAmortizacao :: ValorFinanciado -> TaxaMensal -> QntdParcelas -> ValorParcela -> [(IdParcela, ValorParcela, JurosParcela, ValorAmortizado, SaldoDevedor)]
gerarTabelaAmortizacao valorFinanciado taxaMensal qntdParcelas parcela = gerarParcelas valorFinanciado 1
  where
    taxa = taxaMensal / 100
    
    gerarParcelas :: SaldoDevedor -> IdParcela -> [(IdParcela, ValorParcela, JurosParcela, ValorAmortizado, SaldoDevedor)]
    gerarParcelas saldoDevedor i
      | i > qntdParcelas = []
      | otherwise =
          let juros = saldoDevedor * taxa
              amortizacao = parcela - juros
              saldoDevedorNovo = saldoDevedor - amortizacao
          in (i, parcela, juros, amortizacao, saldoDevedorNovo) : gerarParcelas saldoDevedorNovo (i + 1)

-- Adicionar simulação ao histórico
adicionarSimulacao :: ValorObjeto -> ValorEntrada -> QntdParcelas -> TaxaMensal -> State Historico Simulacao
adicionarSimulacao valorObjeto valorEntrada qntdParcelas taxaMensal = do
  let valorFinanciado = valorObjeto - valorEntrada
      parcela = calcularParcelaFixa valorFinanciado taxaMensal qntdParcelas
      (valorTotalPago, custoEfetivoTotal) = calcularCETeTotalPago parcela valorFinanciado qntdParcelas
      simulacao = Simulacao valorObjeto valorEntrada qntdParcelas taxaMensal valorFinanciado parcela custoEfetivoTotal valorTotalPago
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
        putStrLn "Digite o valor da valorEntrada: "
        hFlush stdout
        valorEntrada <- readLn
        putStrLn "Digite o número total de qntdParcelas: "
        hFlush stdout
        qntdParcelas <- readLn
        putStrLn "Digite a taxa de juros mensal (em porcentagem, por exemplo, 1 para 1%): "
        hFlush stdout
        taxa <- readLn
        let simulacao = evalState (adicionarSimulacao valorObjeto valorEntrada qntdParcelas taxa) historico
        let parcela = calcularParcelaFixa (valorObjeto - valorEntrada) taxa qntdParcelas
        let tabela = gerarTabelaAmortizacao (valorObjeto - valorEntrada) taxa qntdParcelas parcela
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
