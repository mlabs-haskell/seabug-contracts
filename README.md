# seabug-contracts

A library for interacting with Seabug smart contracts via the Cardano Transaction Lib (CTL).

## Tests

Use `spago test` to run the tests. Something like `nix build .#checks.<system>.seabug-contracts` can also be used, where `<system>` is something like `x86_64-linux`.

## Minting

The minting process currently requires some manual steps. To mint a new NFT:

- Upload a new image to nft.storage (e.g. using `seabug/scripts/mint-nft.sh`)
- Uncomment [this line](https://github.com/mlabs-haskell/seabug-contracts/blob/cda88824f87e0b961b738c66a428b7ade77454be/index.js#L39)
- Update the image info [here](https://github.com/mlabs-haskell/seabug-contracts/blob/cda88824f87e0b961b738c66a428b7ade77454be/src/Seabug/Seabug.purs#L34)
  - Make sure you're using the base36 encoded CID (`mint-nft.sh` prints this out)
- Run `make run-dev` and open the link from the console in chrome; this will trigger the minting
  - If Nami/Gero are giving you trouble, this snippet can be used to use a key wallet instead:
    ```
    privateKey <- liftM (error "Failed to parse private key") $
      privateKeyFromBytes
          =<< hexToRawBytes "<secret key>"
    let wallet = Just $ mkKeyWallet (wrap privateKey) Nothing
    ```
  - The secret key can be obtained through e.g. `seabug/scripts/prepare-wallet.sh` (make sure to add ada to that wallet)
    - Note you may have to remove the "5820" from the start of the "cborHex" in the skey file
- Add the wallet that you minted with as an artist to the
  `nft-marketplace-server` database with `admin/create_artist`
