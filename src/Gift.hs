{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Gift
  ( giftSBS,
    giftSerialised,
  )
where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Ledger
  ( Address,
    Validator,
    ValidatorHash,
    mkValidatorScript,
    scriptAddress, ScriptContext
  )
import Ledger.Typed.Scripts as Scripts
    ( Validator,
      mkTypedValidator,
      validatorScript,
      wrapValidator,
      TypedValidator,
      ValidatorTypes(..) )
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx
import PlutusTx.Prelude (Integer, BuiltinData, ($), (.), (==), Bool(..))

data Gift
instance Scripts.ValidatorTypes Gift where
    type instance DatumType Gift = Integer
    type instance RedeemerType Gift = Integer

{-# INLINEABLE mkValidator #-}
mkValidator :: Integer -> Integer -> ScriptContext -> Bool
mkValidator d _ _ = d == 10

typedValidator :: Scripts.TypedValidator Gift
typedValidator = Scripts.mkTypedValidator @Gift $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @Integer @Integer

validator :: Validator
validator = validatorScript typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

giftScript :: Plutus.Script
giftScript = Plutus.unValidatorScript validator

giftSBS :: SBS.ShortByteString
giftSBS = SBS.toShort . LBS.toStrict $ serialise giftScript

giftSerialised :: PlutusScript PlutusScriptV1
giftSerialised = PlutusScriptSerialised giftSBS
