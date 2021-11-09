import Cardano.Api
import Sale
  ( saleSBS,
    saleSerialised,
  )
import qualified Plutus.V1.Ledger.Api as Plutus
import Prelude

main :: IO ()
main = do
  case Plutus.defaultCostModelParams of
    Just m ->
      let (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m saleSBS []
       in do
            print ("Log output" :: String) >> print logout
            case e of
              Left evalErr -> print ("Eval Error" :: String) >> print evalErr
              Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
    Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope "files/sale.plutus" Nothing saleSerialised
  case result of
    Left err -> print $ displayError err
    Right () -> return ()