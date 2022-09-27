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

## Listing (aka selling)

To put a collection NFT from your wallet on the marketplace:

- Uncomment [this line](https://github.com/mlabs-haskell/seabug-contracts/blob/df982074de50d79f18dab20f5fc55d0cf406ba67/index.js#L56) to run the sell contract on page load
- Update [the sell function](https://github.com/mlabs-haskell/seabug-contracts/blob/df982074de50d79f18dab20f5fc55d0cf406ba67/src/Seabug/Seabug.purs#L32-L37) with details of the collection NFT in the wallet
  - The currency symbol and token name will be logged to the console when the NFT is bought, and they can be copied here
- Rebuild with `make run-build` and reload the local running Seabug website, this will trigger the listing transaction
  - Alternatively, use `make run-dev` in the same way as with minting
- Reloading the page again should now show the NFT in the marketplace
- Re-comment the above line to stop running the sell contract on page load
