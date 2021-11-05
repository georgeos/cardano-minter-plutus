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
    BuiltinData,
    Eq ((==)),
    traceIfFalse,
    ($),
    (&&),
    (.),
    (||),
  )

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> ScriptContext -> Bool
mkPolicy _ ctx =
  traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(cs, tn, amt)] -> cs == ownCurrencySymbol ctx && checkTokenName tn && amt == 1
      _ -> False

    checkTokenName :: TokenName -> Bool
    checkTokenName tn =
      tn == TokenName "MyPlutusToken1"
        || tn == TokenName "MyPlutusToken2"
        || tn == TokenName "MyPlutusToken3"
        || tn == TokenName "MyPlutusToken4"
        || tn == TokenName "MyPlutusToken5"

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