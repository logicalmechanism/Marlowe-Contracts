-- A wallet starts a start lottery which allows N wallets to contribute k ADA.
-- At the then of a predefined period a random wallet is paid all ADA in the lottry.
--
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
import System.Random


-- | DEFAULT Parameters for the Lottery. Ends in 50 Slots with 5 players.
theLottery :: Lottery
theLottery = Lottery
    { lotteryDeadline = 50
    , lotteryCollectionDeadline = 60
    , lotteryPlayers = [(player x) | x <- [1..5] ]
    }

-- Creates a pubkeyhask for a wallet.
player :: Integer -> PubKeyHash
player id = (pubKeyHash $ Emulator.walletPubKey (Emulator.Wallet id))

-- RNG betwen lo and hi.
getRn :: (RandomGen g) => Integer -> Integer -> g -> (Integer, g)
getRn lo hi g = randomR (lo, hi) g

-- number of players stored in the lottery.
--
numberOfPlayers :: [PubKeyHash] -> Integer
numberOfPlayers [] = 0
numberOfPlayers users =  1 + (numberOfPlayers (tail users))

-- | A lottery system.
data Lottery = Lottery
    { lotteryDeadline           :: Slot
    , lotteryCollectionDeadline :: Slot
    , lotteryPlayers              :: [PubKeyHash]
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Lottery

-- Endpoint schema
type LotterySchema =
    BlockchainActions
        .\/ Endpoint "start lottery" ()
        .\/ Endpoint "contribute" ()

-- | Start the lottery or play the lottry.
lotterysystem :: AsContractError e => Lottery -> Contract LotterySchema e ()
lotterysystem c = contribute c `select` startLottery c

-- data
data LotteryData
instance Scripts.ScriptType LotteryData where
    type instance RedeemerType LotteryData = ()
    type instance DatumType LotteryData = ()

scriptInstance :: Lottery -> Scripts.ScriptInstance LotteryData
scriptInstance cmp = Scripts.validator @LotteryData
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode cmp)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @()

-- | The validator script.
mkValidator :: Lottery -> () -> () -> ValidatorCtx -> Bool
mkValidator c _ _ p = True


-- | Each contributer adds one ada into the lottery.
contribute :: AsContractError e => Lottery -> Contract LotterySchema e ()
contribute cmp = do
    () <- endpoint @"contribute"
    contributor <- pubKeyHash <$> ownPubKey
    let inst   = scriptInstance cmp
        tx     = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 1)
                <> Constraints.mustValidateIn (Ledger.interval 1 (lotteryDeadline cmp))
    txid <- fmap txId (submitTxConstraints inst tx)
    utxo <- watchAddressUntil (Scripts.scriptAddress inst) (lotteryCollectionDeadline cmp)
    if Constraints.modifiesUtxoSet tx
    then void (submitTxConstraintsSpending inst utxo tx)
    else pure ()

-- | When the end slot is reached, pay a random wallet.
startLottery :: AsContractError e => Lottery -> Contract LotterySchema e ()
startLottery cmp = do
    let inst = scriptInstance cmp -- pass campaign into instance
    ()             <- endpoint @"start lottery"
    _              <- awaitSlot (lotteryDeadline cmp) -- wait til slot then proceed
    unspentOutputs <- utxoAt (Scripts.scriptAddress inst)
    winner         <- pubKeyHash <$> ownPubKey
    let (randNum, nextGen) = getRn 1 (numberOfPlayers (lotteryPlayers cmp)) (mkStdGen 11235)
        value              = foldMap (Validation.txOutValue . Tx.txOutTxOut . snd) (Map.toList unspentOutputs)
        tx                 = Typed.collectFromScript unspentOutputs ()
                            <> Constraints.mustPayToPubKey winner (Ada.toValue 1) -- Send back lottery creation ADA
                            <> Constraints.mustPayToPubKey ((lotteryPlayers cmp) !! (randNum - 1)) value
    void $ submitTxConstraintsSpending inst unspentOutputs tx


endpoints :: AsContractError e => Contract LotterySchema e ()
endpoints = lotterysystem theLottery

mkSchemaDefinitions ''LotterySchema

$(mkKnownCurrencies [])
