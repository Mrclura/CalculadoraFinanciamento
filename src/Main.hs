module Main where

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

calcularParcelaFixa :: ValorFinanciado -> TaxaMensal -> QntdParcelas -> ValorParcela
calcularParcelaFixa valorFinanciado taxaMensal qntdParcelas =
  let taxa = taxaMensal / 100
  in (valorFinanciado * taxa * (1 + taxa) ^ fromIntegral qntdParcelas) / ((1 + taxa) ^ fromIntegral qntdParcelas - 1)

calcularCETeTotalPago :: ValorParcela -> ValorFinanciado -> QntdParcelas -> (ValorTotalPago, CustoEfetivoTotal)
calcularCETeTotalPago parcela valorFinanciado qntdParcelas =
  let valorTotalPago = fromIntegral qntdParcelas * parcela
      custoEfetivoTotal = ((valorTotalPago - valorFinanciado) / valorFinanciado) * 100
  in (valorTotalPago, custoEfetivoTotal)

gerarTabelaAmortizacao :: ValorFinanciado -> TaxaMensal -> QntdParcelas -> ValorParcela -> [(IdParcela, ValorParcela, JurosParcela, ValorAmortizado, SaldoDevedor)]
gerarTabelaAmortizacao valorFinanciado taxaMensal qntdParcelas parcela = gerarParcelas valorFinanciado 1
  where
    taxa = taxaMensal / 100
    gerarParcelas saldoDevedor i
      | i > qntdParcelas = []
      | otherwise =
          let juros = saldoDevedor * taxa
              amortizacao = parcela - juros
              saldoDevedorNovo = saldoDevedor - amortizacao
          in (i, parcela, juros, amortizacao, saldoDevedorNovo) : gerarParcelas saldoDevedorNovo (i + 1)

formatarDouble :: Double -> String
formatarDouble = printf "%.2f"

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
  let parcela = calcularParcelaFixa valorFinanciado taxa qntdParcelas
  let (valorTotalPago, custoEfetivoTotal) = calcularCETeTotalPago parcela valorFinanciado qntdParcelas
  let simulacao = Simulacao valorObjeto valorEntrada qntdParcelas taxa valorFinanciado parcela custoEfetivoTotal valorTotalPago
  let tabela = gerarTabelaAmortizacao valorFinanciado taxa qntdParcelas parcela
  putStrLn $ "Valor financiado: " ++ formatarDouble valorFinanciado
  putStrLn $ "Parcela mensal: " ++ formatarDouble parcela
  putStrLn $ "Custo efetivo total (CET): " ++ formatarDouble custoEfetivoTotal ++ "%"
  putStrLn $ "Valor total pago: " ++ formatarDouble valorTotalPago
  putStrLn "\n Tabela de amortização:"
  putStrLn "Parcela\tValor\tJuros\tAmortização\tSaldo Devedor"
  mapM_ (\(parcelaNum, valor, juros, amortizacao, saldoDevedor) ->
           putStrLn $ printf "%d\t%s\t%s\t%s\t\t%s"
             parcelaNum (formatarDouble valor) (formatarDouble juros) (formatarDouble amortizacao) (formatarDouble saldoDevedor)) tabela
