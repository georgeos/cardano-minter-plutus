import Cardano.Api
import Gift
  ( giftSBS,
    giftSerialised,
  )
import qualified Plutus.V1.Ledger.Api as Plutus
import Prelude

main :: IO ()
main = do
  case Plutus.defaultCostModelParams of
    Just m ->
      let (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m giftSBS []
       in do
            print ("Log output" :: String) >> print logout
            case e of
              Left evalErr -> print ("Eval Error" :: String) >> print evalErr
              Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
    Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope "files/gift.plutus" Nothing giftSerialised
  case result of
    Left err -> print $ displayError err
    Right () -> return ()