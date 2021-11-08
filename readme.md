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
    --tx-in daf599b26807f5aba81d2289e0045180f4d1c7edfe97f0b0a9c6fca1e58d212e#0 \
    --tx-out addr_test1qquw7k22ae7xf772l4ljuvp7ncsq4uzsl2zlvtqlr5kf9lclzytcyjujtzmedxzjgq92kwg48my4dsdnzcmdj6eh5sxqy4j3up+2000000+"1 ecdc4ef8f7bc163723160e410b4008cd18d2c7ae0a666511fa522531.nano5"+"1 074c6f56fb674724cdc5b744027d9d8eb0056b59272b3e344205d349.nft1"+" 1 074c6f56fb674724cdc5b744027d9d8eb0056b59272b3e344205d349.nft2" \
    --change-address addr_test1qzr8dxy9wy4lyjsrmhufkrcfhdxy27k329z66xs6xqjartzrym79ewvl0rem9r0wk8dtry43hj4nt0ghw09n60v40k3srv5uq3 \
    --mint "1 ecdc4ef8f7bc163723160e410b4008cd18d2c7ae0a666511fa522531.nano5" \
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
    --tx-in daf599b26807f5aba81d2289e0045180f4d1c7edfe97f0b0a9c6fca1e58d212e#0 \
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

## Combined transaction: minting a token and redeeming from validator
1. Build transaction
```
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic  1097911063 \
    --tx-in f936356b04f3b8522bc42081899a1656e224a2867679d843cbbbebb2d7145b06#1 \
    --tx-in-script-file files/gift.plutus \
    --tx-in-datum-value 10 \
    --tx-in-redeemer-value 0 \
    --tx-in f936356b04f3b8522bc42081899a1656e224a2867679d843cbbbebb2d7145b06#0 \
    --tx-out addr_test1qquw7k22ae7xf772l4ljuvp7ncsq4uzsl2zlvtqlr5kf9lclzytcyjujtzmedxzjgq92kwg48my4dsdnzcmdj6eh5sxqy4j3up+2000000+"1 184fa5499de7323ea427dd121bd01f3cffb27c8fd8f5eef52acbee48.MyPlutusToken2" \
    --mint "1 184fa5499de7323ea427dd121bd01f3cffb27c8fd8f5eef52acbee48.MyPlutusToken2" \
    --mint-script-file files/minting-policy.plutus \
    --mint-redeemer-file conf/redeemer.json \
    --metadata-json-file conf/metadata.json \
    --change-address addr_test1qzr8dxy9wy4lyjsrmhufkrcfhdxy27k329z66xs6xqjartzrym79ewvl0rem9r0wk8dtry43hj4nt0ghw09n60v40k3srv5uq3 \
    --tx-in-collateral 09a3d2115eaf7a6bb44ca5adad62170ef737a032728c44b9e219f260085b4db8#1 \
    --protocol-params-file conf/protocol-params.json \
    --out-file files/combined.raw
```

2. Sign the transaction
```
cardano-cli transaction sign \
    --tx-body-file files/combined.raw \
    --signing-key-file keys/payment.skey \
    --out-file files/combined.signed
```

3. Submit the transaction
```
cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file files/combined.signed
```

## Combined transaction: minting a token and depositing to validator
1. Build transaction
```
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic  1097911063 \
    --tx-in ec3e1b7b05d7826b8586fbf1741d804883ffc40daf8e2b23a3789b4b2ff1fba6#0 \
    --tx-out $(cat files/gift.addr)+1344798 \
    --tx-out-datum-hash 5b4b01a4a3892ea3751793da57f072ae08eec694ddcda872239fc8239e4bcd1b \
    --tx-in ec3e1b7b05d7826b8586fbf1741d804883ffc40daf8e2b23a3789b4b2ff1fba6#0 \
    --tx-out addr_test1qquw7k22ae7xf772l4ljuvp7ncsq4uzsl2zlvtqlr5kf9lclzytcyjujtzmedxzjgq92kwg48my4dsdnzcmdj6eh5sxqy4j3up+2000000+"1 184fa5499de7323ea427dd121bd01f3cffb27c8fd8f5eef52acbee48.MyPlutusToken3" \
    --mint "1 184fa5499de7323ea427dd121bd01f3cffb27c8fd8f5eef52acbee48.MyPlutusToken3" \
    --mint-script-file files/minting-policy.plutus \
    --mint-redeemer-file conf/redeemer.json \
    --metadata-json-file conf/metadata.json \
    --change-address addr_test1qzr8dxy9wy4lyjsrmhufkrcfhdxy27k329z66xs6xqjartzrym79ewvl0rem9r0wk8dtry43hj4nt0ghw09n60v40k3srv5uq3 \
    --tx-in-collateral 09a3d2115eaf7a6bb44ca5adad62170ef737a032728c44b9e219f260085b4db8#1 \
    --protocol-params-file conf/protocol-params.json \
    --out-file files/combined.raw
```

2. Sign the transaction
```
cardano-cli transaction sign \
    --tx-body-file files/combined.raw \
    --signing-key-file keys/payment.skey \
    --out-file files/combined.signed
```

3. Submit the transaction
```
cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file files/combined.signed
```

## Starting the sale

1. 
cardano-cli address build \
    --payment-script-file files/gift.plutus \
    --testnet-magic 1097911063 \
    --out-file files/gift.addr

cardano-cli transaction hash-script-data \
    --script-data-value 0

cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic  1097911063 \
    --tx-in d4a66d071fc67623918e30ca8f5fb28a8d7135b6c2a7e6d14fbb2076fbcf1186#0 \
    --tx-in d4a66d071fc67623918e30ca8f5fb28a8d7135b6c2a7e6d14fbb2076fbcf1186#1 \
    --tx-out $(cat files/gift.addr)+2000000+"1 074c6f56fb674724cdc5b744027d9d8eb0056b59272b3e344205d349.6e667431" \
    --tx-out-datum-hash 03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314 \
    --change-address addr_test1qzr8dxy9wy4lyjsrmhufkrcfhdxy27k329z66xs6xqjartzrym79ewvl0rem9r0wk8dtry43hj4nt0ghw09n60v40k3srv5uq3 \
    --tx-in-collateral 09a3d2115eaf7a6bb44ca5adad62170ef737a032728c44b9e219f260085b4db8#1 \
    --protocol-params-file conf/protocol-params.json \
    --out-file files/start.raw &&
cardano-cli transaction sign \
    --tx-body-file files/start.raw \
    --signing-key-file keys/payment.skey \
    --out-file files/start.signed &&
cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file files/start.signed

## Closing the sale

cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic  1097911063 \
    --tx-in 276efa32bd7f6ffd68c4b9fc2bfac6b9fe90f302051114914ae8f763f9dee1f2#1 \
    --tx-in-script-file files/gift.plutus \
    --tx-in-datum-value 0 \
    --tx-in-redeemer-value 10 \
    --tx-in 276efa32bd7f6ffd68c4b9fc2bfac6b9fe90f302051114914ae8f763f9dee1f2#0 \
    --tx-out addr_test1qzr8dxy9wy4lyjsrmhufkrcfhdxy27k329z66xs6xqjartzrym79ewvl0rem9r0wk8dtry43hj4nt0ghw09n60v40k3srv5uq3+2000000+"1 074c6f56fb674724cdc5b744027d9d8eb0056b59272b3e344205d349.6e667431" \
    --change-address addr_test1qzr8dxy9wy4lyjsrmhufkrcfhdxy27k329z66xs6xqjartzrym79ewvl0rem9r0wk8dtry43hj4nt0ghw09n60v40k3srv5uq3 \
    --tx-in-collateral 09a3d2115eaf7a6bb44ca5adad62170ef737a032728c44b9e219f260085b4db8#1 \
    --protocol-params-file conf/protocol-params.json \
    --out-file files/close.raw &&
cardano-cli transaction sign \
    --tx-body-file files/close.raw \
    --signing-key-file keys/payment.skey \
    --out-file files/close.signed &&
cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file files/close.signed

## Purchase
echo -n nft1 | basenc --base16 | awk '{print tolower($0)}'

cardano-cli transaction hash-script-data \
    --script-data-value 1

cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic  1097911063 \
    --tx-in 318f65d2fd51a9c3ea4f12c19b579f60ea1345ed8230b20174041a61d7b9d8e7#1 \
    --tx-in-script-file files/gift.plutus \
    --tx-in-datum-value 0 \
    --tx-in-redeemer-value 1 \
    --tx-in 318f65d2fd51a9c3ea4f12c19b579f60ea1345ed8230b20174041a61d7b9d8e7#0 \
    --tx-out $(cat files/gift.addr)+4000000+"1 074c6f56fb674724cdc5b744027d9d8eb0056b59272b3e344205d349.6e667431" \
    --tx-out-datum-hash ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25 \
    --tx-out addr_test1qzr8dxy9wy4lyjsrmhufkrcfhdxy27k329z66xs6xqjartzrym79ewvl0rem9r0wk8dtry43hj4nt0ghw09n60v40k3srv5uq3+2000000+"1 58bcd044cf5cdffb47f74e5ab8495b2a51703970256a5ce4159d1645.6e616e6f31" \
    --mint "1 58bcd044cf5cdffb47f74e5ab8495b2a51703970256a5ce4159d1645.6e616e6f31" \
    --mint-script-file files/minting-policy.plutus \
    --mint-redeemer-file conf/redeemer.json \
    --metadata-json-file conf/metadata.json \
    --change-address addr_test1qzr8dxy9wy4lyjsrmhufkrcfhdxy27k329z66xs6xqjartzrym79ewvl0rem9r0wk8dtry43hj4nt0ghw09n60v40k3srv5uq3 \
    --tx-in-collateral 09a3d2115eaf7a6bb44ca5adad62170ef737a032728c44b9e219f260085b4db8#1 \
    --protocol-params-file conf/protocol-params.json \
    --out-file files/purchase.raw

cardano-cli transaction sign \
    --tx-body-file files/purchase.raw \
    --signing-key-file keys/payment.skey \
    --out-file files/purchase.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file files/purchase.signed
