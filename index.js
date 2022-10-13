"use strict";

// This needs to be asynchronous to load the WASM from CSL
//
// You also need to call `spago bundle-module` to generate the module that is
// imported here. From the repository root, run:
//   spago bundle-module -m <MAIN> --to output.js
const seabug = import("./output.js");

/**
 * Calls Seabug Contract 'marketPlaceBuy'.
 * It returns a promise holding no data.
 *
 */
exports.callMarketPlaceBuy = async (config, args) => {
  const sb = await seabug;
  return sb.callMarketPlaceBuy(config)(args)();
};

/**
 * Calls Seabug Contract 'marketPlaceListNft'.
 * Returns a promise holding nft listings.
 *
 */
exports.callMarketPlaceListNft = async (config) => {
  const sb = await seabug;
  return sb.callMarketPlaceListNft(config)();
};

exports.callMarketPlaceFetchNft = async (config, args) => {
  const sb = await seabug;
  return sb.callMarketPlaceFetchNft(config)(args)();
};

/**
 * Returns a promise containing the connected wallet's lovelace balance.
 */
exports.getWalletBalance = async (config) => {
  const sb = await seabug;
  return sb.callGetWalletBalance(config)();
};

/**
 * Returns a promise containing the connected wallet's public key hash.
 */
exports.getWalletPkh = async (config) => {
  const sb = await seabug;
  return sb.callGetWalletPkh(config)();
};

// Uncomment this for manually minting new tokens (see README)
// seabug.then((sb) => sb.mint().then(() => console.log("Done")))

// Uncomment this for manually relisting/selling tokens (see README)
// seabug.then((sb) => sb.sell().then(() => console.log("Done")))
