# MyToken

Small project that allows to mint an asset called `MyPlutusToken` using plutus and metadata.

## Folder structure
- `/app` contains the executable to generate the minting script
- `/conf` configuration files used in `cardano-cli`
- `/files` generated files during the transaction build process
- `/keys` here should be located all key files to sign the transaction
- `/src` source code of the minting policy in plutus

## Instructions

1. Generate the `minting-policy.plutus` using:
`cabal run plutus-minting`

2. Generate the policy id: using
`cardano-cli transaction policyid --script-file files/minting-policy.plutus > files/minting-policy.id`

3. Change conf/metadata.json to use the new policy id

4. Build the transaction
```
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic  1097911063 \
    --tx-in a44a6b7d2b5ad8a1da8aa4a39c37fcb801a7217fd9fe94e6a4ee9324a1ee513f#0 \
    --tx-out addr_test1qquw7k22ae7xf772l4ljuvp7ncsq4uzsl2zlvtqlr5kf9lclzytcyjujtzmedxzjgq92kwg48my4dsdnzcmdj6eh5sxqy4j3up+2000000+"1 184fa5499de7323ea427dd121bd01f3cffb27c8fd8f5eef52acbee48.MyPlutusToken1" \
    --change-address addr_test1qzr8dxy9wy4lyjsrmhufkrcfhdxy27k329z66xs6xqjartzrym79ewvl0rem9r0wk8dtry43hj4nt0ghw09n60v40k3srv5uq3 \
    --mint "1 184fa5499de7323ea427dd121bd01f3cffb27c8fd8f5eef52acbee48.MyPlutusToken1" \
    --mint-script-file files/minting-policy.plutus \
    --mint-redeemer-file conf/redeemer.json \
    --metadata-json-file conf/metadata.json \
    --tx-in-collateral 09a3d2115eaf7a6bb44ca5adad62170ef737a032728c44b9e219f260085b4db8#1 \
    --protocol-params-file conf/protocol-params.json \
    --out-file files/minting.raw
```

5. Sign the transaction
```
cardano-cli transaction sign \
    --signing-key-file keys/payment.skey \
    --testnet-magic  1097911063 \
    --tx-body-file files/minting.raw \
    --out-file files/minting.signed
```

6. Submit the transaction
```
cardano-cli transaction submit \
    --tx-file files/minting.signed \
    --testnet-magic 1097911063
```

7. Check your token!
> https://testnet.cardanoscan.io/tokenPolicy/258384256362d377a550905d43cb56c405c294a51fe04b211aba1698


## Gift script

1. General gift plutus
`cabal run gift`

2. Generate script address
```
cardano-cli address build \
    --payment-script-file files/gift.plutus \
    --testnet-magic 1097911063 \
    --out-file files/gift.addr
```

3. Query the script address
```
cardano-cli query utxo \
    --testnet-magic 1097911063 \
    --address $(cat files/gift.addr)
```

4. Calculating hash
```
cardano-cli transaction hash-script-data \
    --script-data-value 10
```

5. Depositing to script address minimum required UTxO
```
cardano-cli transaction build \
    --alonzo-era \
    --tx-in e6be5f1d064a3e709f76bf63205150e4c7ee481b666aff81e81f87ec127fe49f#0 \
    --tx-out $(cat files/gift.addr)+1344798 \
    --tx-out-datum-hash 5b4b01a4a3892ea3751793da57f072ae08eec694ddcda872239fc8239e4bcd1b \
    --change-address $(cat keys/payment.addr) \
    --testnet-magic 1097911063 \
    --out-file files/gift.raw
```

7. Signing the transaction
```
cardano-cli transaction sign \
    --tx-body-file files/gift.raw \
    --signing-key-file keys/payment.skey \
    --out-file files/gift.signed
```

8. Submitting the transaction
```
cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file files/gift.signed
```

9. Redeem
```
cardano-cli transaction build \
    --alonzo-era \
    --protocol-params-file conf/protocol-params.json \
    --tx-in 4af8c0034fa6cdae0f37ffc9afcc29754d34b506f3e68b5ee3e539f2bed31cc0#1 \
    --tx-in-script-file files/gift.plutus \
    --tx-in-datum-value 10 \
    --tx-in-redeemer-value 10 \
    --tx-in-collateral 09a3d2115eaf7a6bb44ca5adad62170ef737a032728c44b9e219f260085b4db8#1 \
    --change-address $(cat keys/payment.addr) \
    --testnet-magic 1097911063 \
    --out-file files/redeem.raw
```

10. Sign
```
cardano-cli transaction sign \
    --tx-body-file files/redeem.raw \
    --signing-key-file keys/payment.skey \
    --out-file files/redeem.signed
```

11. Submit
```
cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file files/redeem.signed
```