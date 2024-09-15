module Main where

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
  -- Na tabela Price, o valor de parcela é constante e o valor de amortização é variável
  let parcelaPrice = calcularParcelaPrice valorFinanciado taxa qntdParcelas
  let tabelaPrice = gerarTabelaPrice valorFinanciado taxa qntdParcelas parcelaPrice
  let valorTotalPagoPrice = calcularValorTotalPago tabelaPrice
  let custoEfetivoTotalPrice = calcularCET valorTotalPagoPrice valorFinanciado

  -- Cálculos para a tabela SAC
  -- Na tabela SAC, o valor de amortização é constante e o valor de parcela é variável
  let tabelaSAC = gerarTabelaSAC valorFinanciado taxa qntdParcelas
  let valorTotalPagoSAC = calcularValorTotalPago tabelaSAC
  let custoEfetivoTotalSAC = calcularCET valorTotalPagoSAC valorFinanciado

  -- Exibição das tabelas
  putStrLn $ "\n\nValor financiado: " ++ formatarDouble valorFinanciado 
  putStrLn $ "\n---- SIMULAÇÃO PRICE ----"
  putStrLn $ "Parcela inicial: " ++ formatarDouble (snd5 (head tabelaPrice)) 
  putStrLn $ "Custo efetivo total (CET): " ++ formatarDouble custoEfetivoTotalPrice ++ "%" 
  putStrLn $ "Valor total pago: " ++ formatarDouble valorTotalPagoPrice
  exibirTabelaAmortizacao tabelaPrice
  putStrLn $ "\n\n---- SIMULAÇÃO SAC ----"
  putStrLn $ "Parcela inicial: " ++ formatarDouble (snd5 (head tabelaSAC)) 
  putStrLn $ "Custo efetivo total (CET): " ++ formatarDouble custoEfetivoTotalSAC ++ "%" 
  putStrLn $ "Valor total pago: " ++ formatarDouble valorTotalPagoSAC
  exibirTabelaAmortizacao tabelaSAC

  -- Exibição do comparativo de parcelas e juros
  putStrLn $ "\n\n---- Resumo comparativo ----"
  putStrLn $ "Tabela Price: Valor Total Pago: " ++ formatarDouble valorTotalPagoPrice ++ ", CET: " ++ formatarDouble custoEfetivoTotalPrice ++ "%"
  putStrLn $ "Tabela SAC: Valor Total Pago: " ++ formatarDouble valorTotalPagoSAC ++ ", CET: " ++ formatarDouble custoEfetivoTotalSAC ++ "%"

  if valorTotalPagoPrice > valorTotalPagoSAC
    then putStrLn "Você pagará mais com a Tabela Price."
    else if valorTotalPagoPrice < valorTotalPagoSAC
      then putStrLn "Você pagará mais com a Tabela SAC."
      else putStrLn "O valor total pago é o mesmo para ambas as tabelas."
  exibirComparativoParcelasJuros (gerarTabelaParcelasJuros tabelaPrice) (gerarTabelaParcelasJuros tabelaSAC)

-- Função auxiliar para extrair o segundo elemento de uma tupla 5 elementos
snd5 :: (a, b, c, d, e) -> b
snd5 (_, b, _, _, _) = b
