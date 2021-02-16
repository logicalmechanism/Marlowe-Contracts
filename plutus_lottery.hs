import           Control.Applicative                  (Applicative (pure))
import           Control.Monad                        (void)
import qualified Data.Map                          as Map
import           Language.Plutus.Contract
import qualified Language.Plutus.Contract.Constraints as Constraints
import qualified Language.Plutus.Contract.Typed.Tx    as Typed
import qualified Language.PlutusTx                    as PlutusTx
import           Language.PlutusTx.Prelude            hiding (Applicative (..), Semigroup (..))
import           Ledger                               (PubKeyHash, TxInfo (..), Validator, ValidatorCtx (..),
                                                       pubKeyHash, txId, valueSpent)
import qualified Ledger                               as Ledger
import qualified Ledger.Ada                           as Ada
import qualified Ledger.Contexts                      as V
import qualified Ledger.Interval                      as Interval
import qualified Ledger.Scripts                       as Scripts
import           Ledger.Slot                          (Slot, SlotRange)
import qualified Ledger.Typed.Scripts                 as Scripts
import           Ledger.Value                         (Value)
import qualified Ledger.Value                         as Value
import           Ledger.Address
import qualified Ledger.Contexts                   as Validation
import qualified Ledger.Tx                         as Tx
import           Playground.Contract
import           Prelude                              (Semigroup (..))
import qualified Prelude                              as Haskell
import qualified Wallet.Emulator                      as Emulator
import Numeric
import qualified Data.ByteString.Char8     as C
import Ledger.AddressMap


-- Temp rando number generator
--
randomNumber :: Integer -> Integer
randomNumber players = (toInteger $ fromEnum $ C.last $ C.pack $ show $ Ledger.pubKeyHash ((Emulator.walletPubKey (Emulator.Wallet players)))) `mod` players

-- number of players stored in the lottery.
numberOfPlayers :: [PubKeyHash] -> Integer
numberOfPlayers [] = 0
numberOfPlayers users =  1 + (numberOfPlayers (tail users))

-- | A crowdfunding campaign.
data Campaign = Campaign
    { campaignDeadline           :: Slot
    -- ^ The date by which the campaign target has to be met
    , campaignCollectionDeadline :: Slot
    -- ^ The date by which the campaign owner has to collect the funds
    , campaignPlayers              :: [PubKeyHash]
    -- ^ Public key of the campaign owner. This key is entitled to retrieve the
    --   funds if the campaign is successful.
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Campaign

-- | Action that can be taken by the participants in this contract. 
data CampaignAction =  Pass

PlutusTx.makeIsData ''CampaignAction
PlutusTx.makeLift ''CampaignAction

type CrowdfundingSchema =
    BlockchainActions
        .\/ Endpoint "schedule collection" ()
        .\/ Endpoint "contribute" ()

data Crowdfunding
instance Scripts.ScriptType Crowdfunding where
    type instance RedeemerType Crowdfunding = CampaignAction
    type instance DatumType Crowdfunding = PubKeyHash

scriptInstance :: Campaign -> Scripts.ScriptInstance Crowdfunding
scriptInstance cmp = Scripts.validator @Crowdfunding
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode cmp)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @PubKeyHash @CampaignAction


-- | The validator script.
mkValidator :: Campaign -> PubKeyHash -> CampaignAction -> ValidatorCtx -> Bool
mkValidator c con act p = case act of
    Pass    -> True

-- | The crowdfunding contract for the 'Campaign'.
crowdfunding :: AsContractError e => Campaign -> Contract CrowdfundingSchema e ()
crowdfunding c = contribute c `select` scheduleCollection c

-- | A sample campaign.
theCampaign :: Campaign
theCampaign = Campaign
    { campaignDeadline = 50
    , campaignCollectionDeadline = 60
    , campaignPlayers = [(player x) | x <- [1..5] ]
    }

player :: Integer -> PubKeyHash
player id = (pubKeyHash $ Emulator.walletPubKey (Emulator.Wallet id))


-- | The "contribute" branch of the contract for a specific 'Campaign'. Exposes
--   an endpoint that allows the user to enter their public key and the
--   contribution. Then waits until the campaign is over, and collects the
--   refund if the funding target was not met.
contribute :: AsContractError e => Campaign -> Contract CrowdfundingSchema e ()
contribute cmp = do
    () <- endpoint @"contribute"
    contributor <- pubKeyHash <$> ownPubKey
    -- add in the a new pubkey hash into the campaign here
    let inst   = scriptInstance cmp
        tx     = Constraints.mustPayToTheScript contributor (Ada.lovelaceValueOf 1)
                <> Constraints.mustValidateIn (Ledger.interval 1 (campaignDeadline cmp))
    txid <- fmap txId (submitTxConstraints inst tx)
    utxo <- watchAddressUntil (Scripts.scriptAddress inst) (campaignCollectionDeadline cmp)
    if Constraints.modifiesUtxoSet tx
    then void (submitTxConstraintsSpending inst utxo tx)
    else pure ()

-- | The campaign owner's branch of the contract for a given 'Campaign'. It
--   watches the campaign address for contributions and collects them if
--   the funding goal was reached in time.
scheduleCollection :: AsContractError e => Campaign -> Contract CrowdfundingSchema e ()
scheduleCollection cmp = do
    let inst = scriptInstance cmp -- pass campaign into instance
    ()             <- endpoint @"schedule collection"
    _              <- awaitSlot (campaignDeadline cmp) -- wait til slot then proceed
    unspentOutputs <- utxoAt (Scripts.scriptAddress inst)
    winner         <- pubKeyHash <$> ownPubKey
    let value = foldMap (Validation.txOutValue . Tx.txOutTxOut . snd) (Map.toList unspentOutputs)
        tx    = Typed.collectFromScript unspentOutputs Pass
                <> Constraints.mustPayToPubKey winner (Ada.toValue 1) -- Send back lottery creation ADA
                <> Constraints.mustPayToPubKey ((campaignPlayers cmp) !! (randomNumber (numberOfPlayers (campaignPlayers cmp)))) value
    void $ submitTxConstraintsSpending inst unspentOutputs tx



endpoints :: AsContractError e => Contract CrowdfundingSchema e ()
endpoints = crowdfunding theCampaign

mkSchemaDefinitions ''CrowdfundingSchema

$(mkKnownCurrencies [])
