# seabug-contracts

A library for interacting with Seabug smart contracts via the Cardano Transaction Lib (CTL).

## Tests

Use `spago test` to run the tests. Something like `nix build .#checks.<system>.seabug-contracts-unit-test` can also be used, where `<system>` is something like `x86_64-linux`.

### Plutip Tests

These need a special environment and so are separated into their own suite. Use `spago test --main Test.Plutip` to the plutip tests. Something like `nix build .#checks.<system>.seabug-contracts-plutip-test` can also be used.

## Minting

The minting process currently requires some manual steps. To mint a new NFT:

- Upload a new image to nft.storage (e.g. using `seabug/scripts/mint-nft.sh`). [Pixabay](https://pixabay.com/photos/) is a good source of photos.
- Uncomment [this line](https://github.com/mlabs-haskell/seabug-contracts/blob/cda88824f87e0b961b738c66a428b7ade77454be/index.js#L39)
- Update the image info [here](https://github.com/mlabs-haskell/seabug-contracts/blob/cda88824f87e0b961b738c66a428b7ade77454be/src/Seabug/Seabug.purs#L34)
  - Make sure you're using the base36 encoded CID (`mint-nft.sh` prints this out)
- Run `make run-dev` and open the link from the console in chrome; this will trigger the minting
  - If Nami/Gero are giving you trouble, this snippet can be used to use a key wallet instead:
    ```
    privateKey <- liftM (error "Failed to parse private key") $
      privateKeyFromBytes
          =<< hexToRawBytes "<secret key>"
    privateStakeKey <- liftM (error "Failed to parse private stake key")
      $ privateKeyFromBytes
      =<< hexToRawBytes "<secret stake key>"
    let wallet = Just $ mkKeyWallet (wrap privateKey) (Just $ wrap privateStakeKey)
    ```
  - The secret key can be obtained through e.g. `seabug/scripts/prepare-wallet.sh` (make sure to add ada to that wallet)
    - Note you may have to remove the "5820" from the start of the "cborHex" in the skey file
  - The stake key will also be necessary for minting, the command `cardano-cli stake-address key-gen --signing-key-file stake.skey --verification-key-file stake.vkey` can be used to get a stake key
- Add the wallet that you minted with as an artist to the
  `nft-marketplace-server` database with `admin/create_artist`
