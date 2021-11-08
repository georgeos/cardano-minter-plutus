{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Token
  ( mintTokenSBS,
    mintTokenSerialised,
  )
where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Ledger
  ( Script,
    ScriptContext (scriptContextTxInfo),
    TxInfo (txInfoMint),
    mkMintingPolicyScript,
    ownCurrencySymbol,
    unMintingPolicyScript,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value (TokenName (TokenName), flattenValue)
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool (False),
    BuiltinByteString,
    BuiltinData,
    Eq ((==)),
    Integer,
    consByteString,
    elem,
    emptyByteString,
    otherwise,
    quotient,
    remainder,
    traceIfFalse,
    ($),
    (&&),
    (+),
    (.),
    (<>),
    appendByteString,
  )

{-# INLINEABLE integerToBS #-}
integerToBS :: Integer -> BuiltinByteString
integerToBS x
  | x `quotient` 10 == 0 = digitToBS x
  | otherwise = integerToBS (x `quotient` 10) <> digitToBS (x `remainder` 10)
  where
    digitToBS :: Integer -> BuiltinByteString
    digitToBS d = consByteString (d + 48) emptyByteString

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> ScriptContext -> Bool
mkPolicy _ ctx =
  traceIfFalse "Wrong minted value" checkMintedValue
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkMintedValue :: Bool
    checkMintedValue = case flattenValue (txInfoMint info) of
      [(cs, tn, amt)] -> cs == ownCurrencySymbol ctx && checkTokenName tn && amt == 1
      _ -> False

    checkTokenName :: TokenName -> Bool
    checkTokenName tn = tn `elem` tokenList

    tokenList :: [TokenName]
    tokenList = [TokenName $ appendByteString "nano" (integerToBS i) | i <- numberList]

    numberList :: [Integer]
    numberList = [1,2,3,4,5]

policy :: Scripts.MintingPolicy
policy =
  mkMintingPolicyScript $$(PlutusTx.compile [||Scripts.wrapMintingPolicy mkPolicy||])

plutusScript :: Script
plutusScript =
  unMintingPolicyScript policy

mintTokenSBS :: SBS.ShortByteString
mintTokenSBS = SBS.toShort . LB.toStrict $ serialise plutusScript

mintTokenSerialised :: PlutusScript PlutusScriptV1
mintTokenSerialised = PlutusScriptSerialised mintTokenSBS