{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
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
    Datum (..),
    DatumHash,
    ScriptContext,
    TxInfo (txInfoMint),
    TxOut,
    Validator,
    ValidatorHash,
    Value,
    findDatum,
    findOwnInput,
    getContinuingOutputs,
    mkValidatorScript,
    ownHashes,
    scriptAddress,
    scriptContextTxInfo,
    txInInfoResolved,
    txInfoOutputs,
    txOutDatum,
    txOutValue,
    valuePaidTo,
  )
import Ledger.Ada as Ada (lovelaceValueOf)
import Ledger.Typed.Scripts as Scripts
  ( TypedValidator,
    Validator,
    ValidatorTypes (..),
    mkTypedValidator,
    validatorScript,
    wrapValidator,
  )
import qualified Plutus.V1.Ledger.Scripts as Plutus
import Plutus.V1.Ledger.Value
  ( AssetClass (AssetClass),
    CurrencySymbol (CurrencySymbol),
    TokenName (..),
    assetClass,
    assetClassValue,
    assetClassValueOf,
    currencySymbol,
    geq,
    symbols,
    tokenName,
    unAssetClass,
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool (..),
    BuiltinByteString,
    BuiltinData,
    Integer,
    Maybe (..),
    appendByteString,
    consByteString,
    decodeUtf8,
    emptyByteString,
    fromMaybe,
    head,
    isJust,
    length,
    otherwise,
    quotient,
    remainder,
    traceError,
    traceIfFalse,
    ($),
    (&&),
    (+),
    (.),
    (<),
    (<>),
    (==),
    (>=),
  )

{-# INLINEABLE integerToBS #-}
integerToBS :: Integer -> BuiltinByteString
integerToBS x
  | x `quotient` 10 == 0 = digitToBS x
  | otherwise = integerToBS (x `quotient` 10) <> digitToBS (x `remainder` 10)
  where
    digitToBS :: Integer -> BuiltinByteString
    digitToBS d = consByteString (d + 48) emptyByteString

{-# INLINEABLE tokenNumber #-}
tokenNumber :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
tokenNumber o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d

data Gift

instance Scripts.ValidatorTypes Gift where
  type DatumType Gift = Integer
  type RedeemerType Gift = Integer

-- pubkeyhash to redeem

{-# INLINEABLE mkValidator #-}
mkValidator :: AssetClass -> CurrencySymbol -> Integer -> Integer -> ScriptContext -> Bool
mkValidator id nc datum redeemer ctx
  | redeemer == 10 = True
  | redeemer == 1 =
    traceIfFalse "Wrong minted value" checkMintedValue
      && traceIfFalse "Fees not paid" checkFees
      && traceIfFalse "Identifier nft missing from input" inputHasToken
      && traceIfFalse "Identifier nft missing from output" outputHasToken
      && traceIfFalse "Datum not increased in one" checkOutputDatum
  | otherwise = False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    mintedValue :: Value
    mintedValue = txInfoMint info

    checkMintedValue :: Bool
    checkMintedValue = assetClassValue (assetClass nc $ TokenName $ appendByteString "nano" $ integerToBS $ datum + 1) 1 == mintedValue

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "input missing"
      Just i -> txInInfoResolved i

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) id == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one output"

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) id == 1

    outputDatum :: Integer
    outputDatum = fromMaybe 999 $ tokenNumber ownOutput (`findDatum` info)

    checkOutputDatum :: Bool
    checkOutputDatum =
      outputDatum == datum + 1
        && outputDatum < 6

    checkFees :: Bool
    checkFees =
      let inVal = txOutValue ownInput
          outVal = txOutValue ownOutput
       in outVal `geq` (inVal <> Ada.lovelaceValueOf 2_000_000)

identifier :: AssetClass
identifier = assetClass ("074c6f56fb674724cdc5b744027d9d8eb0056b59272b3e344205d349" :: CurrencySymbol) ("nft1" :: TokenName)

nanoCurrency :: CurrencySymbol
nanoCurrency = "58bcd044cf5cdffb47f74e5ab8495b2a51703970256a5ce4159d1645" :: CurrencySymbol

typedValidator :: Scripts.TypedValidator Gift
typedValidator =
  Scripts.mkTypedValidator @Gift
    ( $$(PlutusTx.compile [||mkValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode identifier
        `PlutusTx.applyCode` PlutusTx.liftCode nanoCurrency
    )
    $$(PlutusTx.compile [||wrap||])
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
