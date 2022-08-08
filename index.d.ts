export function callMarketPlaceBuy(config: Config, args: BuyNftArgs):
    Promise<void>
export function callMarketPlaceListNft(config: Config):
    Promise<Array<NftListing>>
/**
 * Fetch the info for a single NFT. Returns null if the given
 * transaction input has been spent (for example if the NFT has been
 * bought).
 */
export function callMarketPlaceFetchNft(config: Config, args: FetchNftArgs):
    Promise<NftListing?>
export function connectWallet(): Promise<any>
export function getWalletBalance(config: Config): Promise<any>

export type NetworkId
    = 0 // Testnet
    | 1 // Mainnet

export type Config = {
    serverHost: string,
    serverPort: number,
    // If CTL Haskell server uses SSL
    serverSecureConn: boolean,
    ogmiosHost: string,
    ogmiosPort: number,
    // If Ogmios uses SSL
    ogmiosSecureConn: boolean,
    datumCacheHost: string,
    datumCachePort: number,
    // If ogmios-datum-cache uses SSL
    datumCacheSecureConn: boolean,
    networkId: NetworkId,
    // blockfrost.io API key
    projectId: string,
}

export type ContractArgs = {
    nftCollectionArgs: NftCollectionArgs,
    nftIdArgs: NftIdArgs
}

export type BuyNftArgs = {

}

export type FetchNftArgs = Input

export type NftCollectionArgs = {
    // CurrencySymbol of nft collection
    collectionNftCs: string,
    lockLockup: bigint,
    lockLockupEnd: bigint,
    // ValidatorHash of a script locking the nft
    lockingScript: string,
    //PaymentPubKeyHash of the nft author
    author: string,
    authorShare: bigint,
    // Validator hash
    daoScript: string,
    daoShare: bigint
}

export type NftIdArgs = {
    // TokenName of the nft collection
    collectionNftTn: string,
    price: bigint,
    // PaymentPubKeyHash of the nft current owner
    owner: string
}

export type NftListing = {
    input: Input,
    output: Output,
    metadata: Metadata
}

export type Input = {
    transactionId: String,
    inputIndex: number
}

export type Output = {
    address: string,
    value: ValueOut,
    data_hash: string
}

export type ValueOut = {
    currencySymbol: string,
    tokenName: string,
    amount: bigint
}

export type Metadata = {
    seabugMetadata: SeabugMetadata
    ipfsHash: string
}

export type SeabugMetadata = {
    // Hash of minting policy
    policyId: string,
    // Hexadecimal string representing bytes of minting policy
    mintPolicy: string,
    // Currency symbol
    collectionNftCS: string,
    // Token name
    collectionNftTN: string,
    // Hash of locking script
    lockingScript: string,
    // Pub key hash of author
    authorPkh: string,
    authorShare: bigint,
    // Hash of marketplace validator
    marketplaceScript: string,
    marketplaceShare: bigint,
    // Pub key hash of owner
    ownerPkh: string,
    ownerPrice: bigint
}
