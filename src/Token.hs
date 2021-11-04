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
import Codec.Serialise ( serialise )
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Ledger
    ( ownCurrencySymbol,
      mkMintingPolicyScript,
      unMintingPolicyScript,
      ScriptContext(scriptContextTxInfo),
      TxInfo(txInfoMint),
      Script )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value ( TokenName(TokenName), flattenValue )
import qualified PlutusTx
import PlutusTx.Prelude
    ( Bool(False),
      Integer,
      (.),
      BuiltinData,
      (&&),
      ($),
      traceIfFalse,
      Eq((==)) )

{-# INLINEABLE mkPolicy #-}
mkPolicy :: Integer -> BuiltinData -> ScriptContext -> Bool
mkPolicy _ _ ctx = traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(cs, tn, amt)] -> cs == ownCurrencySymbol ctx && tn == TokenName "MyPlutusToken" && amt == 1
      _ -> False

policy :: Integer -> Scripts.MintingPolicy
policy amount =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode amount

plutusScript :: Script
plutusScript =
  unMintingPolicyScript (policy amount)
  where
    amount = 1

mintTokenSBS :: SBS.ShortByteString
mintTokenSBS = SBS.toShort . LB.toStrict $ serialise plutusScript

mintTokenSerialised :: PlutusScript PlutusScriptV1
mintTokenSerialised = PlutusScriptSerialised mintTokenSBS