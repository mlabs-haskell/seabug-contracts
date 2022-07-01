-- Feel free to update binary fixtures if they do not match the results you are
-- getting in tests. However, make sure you understand the reason why they
-- don't match.
-- To update the fixture, simply copy the value from failing test output.
--
-- Or construct a value using CSL and get the hex string:
--
-- ```
-- const byteArrayToHex = arr => Buffer.from(arr).toString('hex');
-- console.log(byteArrayToHex(something.to_bytes()))
-- ```
module Test.Fixtures
  ( seabugMetadataDeltaFixture1
  , seabugMetadataFixture1
  ) where

import Prelude

import Contract.Address (Ed25519KeyHash, PubKeyHash(..))
import Contract.Numeric.Natural as Natural
import Contract.Prelude (fromJust)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Scripts (MintingPolicyHash(..), ScriptHash, ValidatorHash(..))
import Contract.Value (TokenName, mkTokenName)
import Data.BigInt as BigInt
import Metadata.Seabug (SeabugMetadata(..), SeabugMetadataDelta(..))
import Metadata.Seabug.Share (Share, mkShare)
import Partial.Unsafe (unsafePartial)
import Serialization.Hash (ed25519KeyHashFromBytes, scriptHashFromBytes)
import Types.RawBytes (hexToRawBytesUnsafe)
import Cardano.Types.Value (CurrencySymbol, mkCurrencySymbol)

currencySymbol1 :: CurrencySymbol
currencySymbol1 = unsafePartial $ fromJust $ mkCurrencySymbol $
  hexToByteArrayUnsafe
    "1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e"

tokenName1 :: TokenName
tokenName1 = unsafePartial $ fromJust $ mkTokenName $
  hexToByteArrayUnsafe "4974657374546f6b656e"

ed25519KeyHashFixture1 :: Ed25519KeyHash
ed25519KeyHashFixture1 =
  -- $ Bech32 "hstk_1rsf0q0q77t5nttxrtmpwd7tvv58a80a686t92pgy65ekz0s8ncu"
  unsafePartial $ fromJust
    $ ed25519KeyHashFromBytes
    $ hexToRawBytesUnsafe
        "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"

ed25519KeyHashFixture2 :: Ed25519KeyHash
ed25519KeyHashFixture2 =
  -- "hbas_1xranhpfej50zdup5jy995dlj9juem9x36syld8wm465hz92acfp"
  unsafePartial $ fromJust
    $ ed25519KeyHashFromBytes
    $ hexToRawBytesUnsafe
        "30fb3b8539951e26f034910a5a37f22cb99d94d1d409f69ddbaea971"

scriptHash1 :: ScriptHash
scriptHash1 = unsafePartial $ fromJust $ scriptHashFromBytes $
  hexToRawBytesUnsafe
    "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65"

scriptHash2 :: ScriptHash
scriptHash2 = unsafePartial $ fromJust $ scriptHashFromBytes $
  hexToRawBytesUnsafe
    "00000000005bb21ce6d8c7502aca70b9316d10e958611f3c6b758f60"

policyId :: MintingPolicyHash
policyId = MintingPolicyHash scriptHash1

validatorHashFixture1 :: ValidatorHash
validatorHashFixture1 = ValidatorHash scriptHash1

validatorHashFixture2 :: ValidatorHash
validatorHashFixture2 = ValidatorHash scriptHash2

shareFixture :: Share
shareFixture = unsafePartial $ fromJust $ mkShare 100

seabugMetadataFixture1 :: SeabugMetadata
seabugMetadataFixture1 = SeabugMetadata
  { policyId: policyId
  , mintPolicy: hexToByteArrayUnsafe "00000000"
  , collectionNftCS: currencySymbol1
  , collectionNftTN: tokenName1
  , lockingScript: validatorHashFixture1
  , authorPkh: PubKeyHash ed25519KeyHashFixture1
  , authorShare: shareFixture
  , marketplaceScript: validatorHashFixture2
  , marketplaceShare: shareFixture
  , ownerPkh: PubKeyHash ed25519KeyHashFixture2
  , ownerPrice: unsafePartial $ fromJust $ Natural.fromBigInt $ BigInt.fromInt
      10
  }

seabugMetadataDeltaFixture1 :: SeabugMetadataDelta
seabugMetadataDeltaFixture1 = SeabugMetadataDelta
  { policyId: policyId
  , ownerPkh: PubKeyHash ed25519KeyHashFixture2
  , ownerPrice: unsafePartial $ fromJust $ Natural.fromBigInt $ BigInt.fromInt
      10
  }
