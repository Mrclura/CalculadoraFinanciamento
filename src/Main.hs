{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Gloss
import Data.List (foldl')
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
exibirTabelaAmortizacao :: [(IdParcela, ValorParcela, JurosParcela, ValorAmortizado, SaldoDevedor)] -> IO ()
exibirTabelaAmortizacao tabela = do
  putStrLn $ "\nTabela de Amortização"
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

-- Função para transformar dados em coordenadas para gloss
toFloatTuple :: (Double, Double) -> (Float, Float)
toFloatTuple (x, y) = (realToFrac x, realToFrac y)

-- Função para desenhar gráficos com gloss
drawChart :: [(Double, ValorParcela)] -> [(Double, ValorParcela)] -> Picture
drawChart scaleData scaleDataSAC = Pictures
  [ color green (line (map toFloatTuple scaleData))
  , color blue (line (map toFloatTuple scaleDataSAC))
  , Pictures [ translate (realToFrac x) (realToFrac y) (color red (circle 5)) | (x, y) <- scaleData ]
  , Pictures [ translate (realToFrac x) (realToFrac y) (color blue (circle 5)) | (x, y) <- scaleDataSAC ]
  ]

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
  let valorFinanciado = valorObjeto - valorEntrada
  
  -- Cálculos para a tabela Price
  let parcelaPrice = calcularParcelaPrice valorFinanciado taxa qntdParcelas
  let tabelaPrice = gerarTabelaPrice valorFinanciado taxa qntdParcelas parcelaPrice
  let valorTotalPagoPrice = calcularValorTotalPago tabelaPrice
  let cetPrice = calcularCET valorTotalPagoPrice valorFinanciado

  -- Cálculos para a tabela SAC
  let tabelaSAC = gerarTabelaSAC valorFinanciado taxa qntdParcelas
  let valorTotalPagoSAC = calcularValorTotalPago tabelaSAC
  let cetSAC = calcularCET valorTotalPagoSAC valorFinanciado

  -- Exibição das tabelas
  exibirTabelaAmortizacao tabelaPrice
  exibirTabelaAmortizacao tabelaSAC
  exibirComparativoParcelasJuros (gerarTabelaParcelasJuros tabelaPrice) (gerarTabelaParcelasJuros tabelaSAC)

  putStrLn $ "\nCusto Efetivo Total (CET) Price: " ++ formatarDouble cetPrice ++ "%"
  putStrLn $ "Custo Efetivo Total (CET) SAC: " ++ formatarDouble cetSAC ++ "%"

  -- Desenhando gráficos
  let scaleData = [(fromIntegral i * 10, valorParcela) | i <- [1..qntdParcelas], let (_, valorParcela, _, _, _) = tabelaPrice !! (i - 1)]
  let scaleDataSAC = [(fromIntegral i * 10, valorParcela) | i <- [1..qntdParcelas], let (_, valorParcela, _, _, _) = tabelaSAC !! (i - 1)]
  display (InWindow "Comparativo de Amortização" (800, 600) (100, 100)) white (drawChart scaleData scaleDataSAC)
