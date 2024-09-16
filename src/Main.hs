{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.List (foldl')
import Control.Monad.State
import System.IO (hFlush, stdout)
import Text.Printf (printf)

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

type SimulacaoState = State [Simulacao]

-- PRICE: Função para calcular o valor da parcela
calcularParcelaPrice :: ValorFinanciado -> TaxaMensal -> QntdParcelas -> ValorParcela
calcularParcelaPrice valorFinanciado taxaMensal qntdParcelas =
  let taxa = taxaMensal / 100
  in (valorFinanciado * taxa * (1 + taxa) ^ fromIntegral qntdParcelas) / ((1 + taxa) ^ fromIntegral qntdParcelas - 1)

-- PRICE: Função para gerar a tabela de amortização
gerarTabelaPrice :: ValorFinanciado -> TaxaMensal -> QntdParcelas -> ValorParcela -> [(IdParcela, ValorParcela, JurosParcela, ValorAmortizado, SaldoDevedor)]
gerarTabelaPrice valorFinanciado taxaMensal qntdParcelas parcela = gerarParcelas valorFinanciado 1
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

-- Função para calcular o valor da parcela SAC
calcularParcelaSAC :: ValorFinanciado -> QntdParcelas -> ValorAmortizado
calcularParcelaSAC valorFinanciado qntdParcelas = valorFinanciado / fromIntegral qntdParcelas

-- Função para gerar a tabela de amortização SAC
gerarTabelaSAC :: ValorFinanciado -> TaxaMensal -> QntdParcelas -> [(IdParcela, ValorParcela, JurosParcela, ValorAmortizado, SaldoDevedor)]
gerarTabelaSAC valorFinanciado taxaMensal qntdParcelas = gerarParcelas valorFinanciado 1
  where
    taxa = taxaMensal / 100
    amortizacao = calcularParcelaSAC valorFinanciado qntdParcelas
    
    gerarParcelas :: SaldoDevedor -> IdParcela -> [(IdParcela, ValorParcela, JurosParcela, ValorAmortizado, SaldoDevedor)]
    gerarParcelas saldoDevedor i
      | i > qntdParcelas = []
      | otherwise =
          let juros = saldoDevedor * taxa
              parcela = amortizacao + juros
              saldoDevedorNovo = saldoDevedor - amortizacao
          in (i, parcela, juros, amortizacao, saldoDevedorNovo) : gerarParcelas saldoDevedorNovo (i + 1)

-- Função para calcular o valor total pago
calcularValorTotalPago :: [(IdParcela, ValorParcela, JurosParcela, ValorAmortizado, SaldoDevedor)] -> ValorTotalPago
calcularValorTotalPago parcelas =
  foldl' (\acc (_, parcela, _, _, _) -> acc + parcela) 0 parcelas

-- Função para calcular o Custo Efetivo Total (CET)
calcularCET :: ValorTotalPago -> ValorFinanciado -> CustoEfetivoTotal
calcularCET valorTotalPago valorFinanciado =
  ((valorTotalPago - valorFinanciado) / valorFinanciado) * 100

-- Formatar números com duas casas decimais
formatarDouble :: Double -> String
formatarDouble = printf "%.2f"

-- Função para exibir a tabela de amortização completa
exibirTabelaAmortizacao :: String -> [(IdParcela, ValorParcela, JurosParcela, ValorAmortizado, SaldoDevedor)] -> IO ()
exibirTabelaAmortizacao titulo tabela = do
  putStrLn $ "\n" ++ titulo
  putStrLn "Parcela\tValor\tJuros\tAmortização\tSaldo Devedor"
  mapM_ (\(parcelaNum, valor, juros, amortizacao, saldoDevedor) -> 
           putStrLn $ printf "%d\t%s\t%s\t%s\t\t%s"
             parcelaNum (formatarDouble valor) (formatarDouble juros) (formatarDouble amortizacao) (formatarDouble saldoDevedor)) tabela

-- Função para exibir o comparativo de parcelas e juros pagos por mês entre Price e SAC
exibirComparativoParcelasJuros :: [(IdParcela, ValorParcela, JurosParcela)] -> [(IdParcela, ValorParcela, JurosParcela)] -> IO ()
exibirComparativoParcelasJuros tabelaPrice tabelaSAC = do
  putStrLn "\n--- Comparativo de Parcelas e Juros Pagos por Parcela (Price vs SAC) ---"
  putStrLn "Mês\t|\tParcela Price\t|\tJuros Price\t|\tParcela SAC\t|\tJuros SAC"
  putStrLn "--------------------------------------------------------------------------"
  mapM_ (\((idPrice, parcelaPrice, jurosPrice), (_, parcelaSAC, jurosSAC)) -> 
            putStrLn $ show idPrice ++ "º Mês\t|\t" ++ formatarDouble parcelaPrice ++ "\t|\t" ++ formatarDouble jurosPrice ++ 
                       "\t|\t" ++ formatarDouble parcelaSAC ++ "\t|\t" ++ formatarDouble jurosSAC) 
        (zip tabelaPrice tabelaSAC)

-- Função auxiliar para gerar uma tabela apenas com parcelas e juros
gerarTabelaParcelasJuros :: [(IdParcela, ValorParcela, JurosParcela, ValorAmortizado, SaldoDevedor)] -> [(IdParcela, ValorParcela, JurosParcela)]
gerarTabelaParcelasJuros tabela = map (\(idParcela, valorParcela, juros, _, _) -> (idParcela, valorParcela, juros)) tabela

-- Função para desenhar gráficos com Chart
drawChart :: [(Int, ValorParcela)] -> [(Int, ValorParcela)] -> IO ()
drawChart tabelaPrice tabelaSAC = do
  toFile def "comparativo.png" $ do
    layout_title .= "Comparativo de Parcelas (Price vs SAC)"
    layout_x_axis . laxis_title .= "Mês"
    layout_y_axis . laxis_title .= "Valor da Parcela"
    plot (line "Parcelas Price" [map (\(x, y) -> (fromIntegral x :: Double, y)) tabelaPrice])
    plot (line "Parcelas SAC" [map (\(x, y) -> (fromIntegral x :: Double, y)) tabelaSAC])

-- Função para adicionar uma simulação ao estado
adicionarSimulacao :: Simulacao -> SimulacaoState ()
adicionarSimulacao simulacao = state $ \simulacoes -> ((), simulacao : simulacoes)

-- Função para coletar dados da simulação e adicionar ao estado
processarSimulacao :: ValorObjeto -> ValorEntrada -> QntdParcelas -> TaxaMensal -> SimulacaoState ()
processarSimulacao valorObjeto valorEntrada qntdParcelas taxa = do
  let valorFinanciado = valorObjeto - valorEntrada
      parcelaPrice = calcularParcelaPrice valorFinanciado taxa qntdParcelas
      tabelaPrice = gerarTabelaPrice valorFinanciado taxa qntdParcelas parcelaPrice
      valorTotalPagoPrice = calcularValorTotalPago tabelaPrice
      cetPrice = calcularCET valorTotalPagoPrice valorFinanciado

      tabelaSAC = gerarTabelaSAC valorFinanciado taxa qntdParcelas
      valorTotalPagoSAC = calcularValorTotalPago tabelaSAC
      cetSAC = calcularCET valorTotalPagoSAC valorFinanciado

      simulacaoPrice = Simulacao valorObjeto valorEntrada qntdParcelas taxa valorFinanciado parcelaPrice cetPrice valorTotalPagoPrice
      simulacaoSAC = Simulacao valorObjeto valorEntrada qntdParcelas taxa valorFinanciado (calcularParcelaSAC valorFinanciado qntdParcelas) cetSAC valorTotalPagoSAC

  adicionarSimulacao simulacaoPrice
  adicionarSimulacao simulacaoSAC

main :: IO ()
main = do
  putStrLn "Digite o valor do objeto financiado: "
  hFlush stdout
  valorObjeto <- readLn
  putStrLn "Digite o valor da entrada: "
  hFlush stdout
  valorEntrada <- readLn
  putStrLn "Digite o número total de parcelas: "
  hFlush stdout
  qntdParcelas <- readLn
  putStrLn "Digite a taxa de juros mensal (em porcentagem, por exemplo, 1 para 1%): "
  hFlush stdout
  taxa <- readLn

  let simulacaoInicial = []
  let ((), simulacoes) = runState (processarSimulacao valorObjeto valorEntrada qntdParcelas taxa) simulacaoInicial

  let tabelaPrice = gerarTabelaPrice (valorObjeto - valorEntrada) taxa qntdParcelas (calcularParcelaPrice (valorObjeto - valorEntrada) taxa qntdParcelas)
      tabelaSAC = gerarTabelaSAC (valorObjeto - valorEntrada) taxa qntdParcelas
      tabelaPriceChart = [(i, valorParcela) | (i, valorParcela, _, _, _) <- tabelaPrice]
      tabelaSACChart = [(i, valorParcela) | (i, valorParcela, _, _, _) <- tabelaSAC]

  putStrLn "\n--- SIMULAÇÃO PRICE ---"
  putStrLn $ "\nParcela Inicial PRICE: " ++ formatarDouble (calcularParcelaPrice (valorObjeto - valorEntrada) taxa qntdParcelas)
  putStrLn $ "Valor Total Pago PRICE: " ++ formatarDouble (calcularValorTotalPago tabelaPrice)
  putStrLn $ "Custo Efetivo Total (CET) PRICE: " ++ formatarDouble (calcularCET (calcularValorTotalPago tabelaPrice) (valorObjeto - valorEntrada)) ++ "%"

  putStrLn "--------------------------------------------------------------------------"
  exibirTabelaAmortizacao "Tabela de Amortização" tabelaPrice

  putStrLn "\n--- Simulação SAC ---"
  putStrLn $ "\nParcela Inicial SAC: " ++ formatarDouble (calcularParcelaSAC (valorObjeto - valorEntrada) qntdParcelas)
  putStrLn $ "Valor Total Pago SAC: " ++ formatarDouble (calcularValorTotalPago tabelaSAC)
  putStrLn $ "Custo Efetivo Total (CET) SAC: " ++ formatarDouble (calcularCET (calcularValorTotalPago tabelaSAC) (valorObjeto - valorEntrada)) ++ "%"

  putStrLn "--------------------------------------------------------------------------"
  exibirTabelaAmortizacao "Tabela de Amortização" tabelaSAC

  -- Exibindo comparativo
  exibirComparativoParcelasJuros (gerarTabelaParcelasJuros tabelaPrice) (gerarTabelaParcelasJuros tabelaSAC)

  -- Desenhando gráficos
  drawChart tabelaPriceChart tabelaSACChart
