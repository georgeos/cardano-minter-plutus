# MyToken

Small project that allows to sale NFTs as assets called `MyTokenXX`, using plutus and metadata, where XX is a counter from 1 to 5.
In every purchase, user should pay 2 ADA and get back the NFT minted.

## Folder structure
- `/app` contains the executable to generate the minting script
- `/conf` configuration files used in `cardano-cli`
- `/files` generated files during the transaction build process
- `/keys` here should be located all key files to sign the transaction
- `/src` source code of the minting policy and validator script in plutus

## Instructions

1. Generate the `minting-policy.plutus`:
    - `cabal run plutus-minting`

2. Generate the policy id
    - `cardano-cli transaction policyid --script-file files/minting-policy.plutus > files/minting-policy.id`

3. Change:
    - `conf/metadata.json` and `src/Sale.hs` to use the new policy id
    - `src/Sale.hs` to use your NFT to identify the sale

4. Generate sale plutus
    - `cabal run sale`

5. Generate script address
```
cardano-cli address build \
    --payment-script-file files/sale.plutus \
    --testnet-magic 1097911063 \
    --out-file files/sale.addr
```

6. Query the script address
```
cardano-cli query utxo \
    --testnet-magic 1097911063 \
    --address $(cat files/sale.addr)
```

7. Start the sale
```
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic  1097911063 \
    --tx-in b03e6076294fc2bd0f058582d3357740f65b5990d2de8d68a86109c5ce7f1631#0 \
    --tx-in b03e6076294fc2bd0f058582d3357740f65b5990d2de8d68a86109c5ce7f1631#1 \
    --tx-out $(cat files/sale.addr)+2000000+"1 074c6f56fb674724cdc5b744027d9d8eb0056b59272b3e344205d349.6e667431" \
    --tx-out-datum-embed-value 0 \
    --change-address $(cat keys/payment.addr) \
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
```

8. Purchase

    8.1 Generate the name of the NFT in the scripts below and `conf/metadata.json` using following command:
    - `echo -n MyToken2 | basenc --base16 | awk '{print tolower($0)}'`

    8.2 Change the following script with appropiated values (currently it's based to buy MyToken2)
    - --tx-in 
    - --tx-in-datum-value
    - --tx-out : ada amount for the script and the asset name in base16 for address
    - --tx-out-datum-embed-value
    - --mint : with the asset name in base16
```
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic  1097911063 \
    --tx-in 5e0ed0389b1477b655bd13eb0bfe16b8948b24a82001f6c4cd45ec682763a9e5#1 \
    --tx-in-script-file files/sale.plutus \
    --tx-in-datum-value 1 \
    --tx-in-redeemer-value 1 \
    --tx-in 5e0ed0389b1477b655bd13eb0bfe16b8948b24a82001f6c4cd45ec682763a9e5#0 \
    --tx-out $(cat files/sale.addr)+6000000+"1 074c6f56fb674724cdc5b744027d9d8eb0056b59272b3e344205d349.6e667431" \
    --tx-out-datum-embed-value 2 \
    --tx-out $(cat keys/payment.addr)+2000000+"1 f13faca3fc4a5964d797ab1a547ad0a7a265b8bae645894b00d94fe9.4d79546f6b656e32" \
    --mint "1 f13faca3fc4a5964d797ab1a547ad0a7a265b8bae645894b00d94fe9.4d79546f6b656e32" \
    --mint-script-file files/minting-policy.plutus \
    --mint-redeemer-file conf/redeemer.json \
    --metadata-json-file conf/metadata.json \
    --change-address $(cat keys/payment.addr) \
    --tx-in-collateral 09a3d2115eaf7a6bb44ca5adad62170ef737a032728c44b9e219f260085b4db8#1 \
    --protocol-params-file conf/protocol-params.json \
    --out-file files/purchase.raw &&
cardano-cli transaction sign \
    --tx-body-file files/purchase.raw \
    --signing-key-file keys/payment.skey \
    --out-file files/purchase.signed &&
cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file files/purchase.signed
```

9. Check the purchase from cardanoscan:
> https://testnet.cardanoscan.io/address/addr_test1wq58k5juzelsu5famg4ln8lqzj0jrtf3l6ge4w8edk0gvcs4fxcl5

10. Close the sale: change the following script with the appropiated values (currently it's based to close with datum = 1):
    - --tx-in
    - --tx-in-datum-value
    - --tx-out
    - --tx-in-collateral
```
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic  1097911063 \
    --tx-in 5e0ed0389b1477b655bd13eb0bfe16b8948b24a82001f6c4cd45ec682763a9e5#1 \
    --tx-in-script-file files/sale.plutus \
    --tx-in-datum-value 1 \
    --tx-in-redeemer-value 10 \
    --tx-in 5e0ed0389b1477b655bd13eb0bfe16b8948b24a82001f6c4cd45ec682763a9e5#0 \
    --tx-out $(cat keys/payment.addr)+2000000+"1 074c6f56fb674724cdc5b744027d9d8eb0056b59272b3e344205d349.6e667431" \
    --change-address $(cat keys/payment.addr) \
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
```