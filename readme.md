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
    --tx-in d40ab3a07bb41d6320ae64b167a600b87af453cd770981f511b406d0db0a29b7#0 \
    --tx-out addr_test1qquw7k22ae7xf772l4ljuvp7ncsq4uzsl2zlvtqlr5kf9lclzytcyjujtzmedxzjgq92kwg48my4dsdnzcmdj6eh5sxqy4j3up+2000000+"1 258384256362d377a550905d43cb56c405c294a51fe04b211aba1698.MyPlutusToken" \
    --change-address addr_test1qzr8dxy9wy4lyjsrmhufkrcfhdxy27k329z66xs6xqjartzrym79ewvl0rem9r0wk8dtry43hj4nt0ghw09n60v40k3srv5uq3 \
    --mint "1 258384256362d377a550905d43cb56c405c294a51fe04b211aba1698.MyPlutusToken" \
    --mint-script-file files/minting-policy.plutus \
    --mint-redeemer-value 1 \
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