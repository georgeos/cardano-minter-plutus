import           Prelude
import           Cardano.Api
import qualified Plutus.V1.Ledger.Api as Plutus
import           Token (mintTokenSerialised,
                   mintTokenSBS)

main :: IO ()
main = do
  case Plutus.defaultCostModelParams of
        Just m ->
          let (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m mintTokenSBS []
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope "files/minting-policy.plutus" Nothing mintTokenSerialised
  case result of
    Left err -> print $ displayError err
    Right () -> return ()