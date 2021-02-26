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
    , lotteryPlayers              :: [PubKeyHash]
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Lottery

-- Endpoint schema
type LotterySchema =
    BlockchainActions
        .\/ Endpoint "start lottery" ()
        .\/ Endpoint "yes" ()
        .\/ Endpoint "no" ()

-- | Start the lottery or play the lottry.
lotterysystem :: AsContractError e => Lottery -> Contract LotterySchema e ()
lotterysystem c = yes c `select` no c `select` startLottery c

-- data
data LotteryData
instance Scripts.ScriptType LotteryData where
    type instance RedeemerType LotteryData = ()
    type instance DatumType LotteryData = ()

-- The validator does not take in any inputs so the data is ().
scriptInstance :: Lottery -> Scripts.ScriptInstance LotteryData
scriptInstance cmp = Scripts.validator @LotteryData
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode cmp)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @()

-- | The validator script. Just pass True since its auto pay by time and RNG.
mkValidator :: Lottery -> () -> () -> ValidatorCtx -> Bool
mkValidator c _ _ p = True


-- | Each contributer adds one ada into the lottery.
no :: AsContractError e => Lottery -> Contract LotterySchema e ()
no cmp = do
    ()         <- endpoint @"no"
    let inst   = scriptInstance cmp
        tx     = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 0)              -- Force 1 ADA buy in
                <> Constraints.mustValidateIn (Ledger.interval 1 (lotteryDeadline cmp)) -- Must buy in before lottery is over
    txid       <- fmap txId (submitTxConstraints inst tx)
    unspentOutputs <- utxoAt (Scripts.scriptAddress inst) 
    if Constraints.modifiesUtxoSet tx
    then void (submitTxConstraintsSpending inst unspentOutputs tx)
    else pure ()

-- | Each contributer adds one ada into the lottery.
yes :: AsContractError e => Lottery -> Contract LotterySchema e ()
yes cmp = do
    ()         <- endpoint @"yes"
    let inst   = scriptInstance cmp
        tx     = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 1)              -- Force 1 ADA buy in
                <> Constraints.mustValidateIn (Ledger.interval 1 (lotteryDeadline cmp)) -- Must buy in before lottery is over
    txid       <- fmap txId (submitTxConstraints inst tx)
    unspentOutputs <- utxoAt (Scripts.scriptAddress inst) 
    if Constraints.modifiesUtxoSet tx
    then void (submitTxConstraintsSpending inst unspentOutputs tx)
    else pure ()

-- | When the end slot is reached, pay a random wallet.
startLottery :: AsContractError e => Lottery -> Contract LotterySchema e ()
startLottery cmp = do
    let inst = scriptInstance cmp
    ()             <- endpoint @"start lottery"
    _              <- awaitSlot (lotteryDeadline cmp)     -- wait til slot then proceed
    unspentOutputs <- utxoAt (Scripts.scriptAddress inst) -- Get ada in script
    logInfo @List (Map.toList unspentOutputs)
    starter        <- pubKeyHash <$> ownPubKey            -- the key that started the lottery
    -- Pay all users that voted for some value here. right now it just picks a random player
    let (randNum, nextGen) = getRn 1 (numberOfPlayers (lotteryPlayers cmp)) (mkStdGen 123)                     -- rando number between 0 and number of players
        value              = foldMap (Validation.txOutValue . Tx.txOutTxOut . snd) (Map.toList unspentOutputs) -- Get value in script
        tx                 = Typed.collectFromScript unspentOutputs ()
                            <> Constraints.mustPayToPubKey starter (Ada.toValue 1)                             -- Send back lottery creation ADA
                            <> Constraints.mustPayToPubKey ((lotteryPlayers cmp) !! (randNum - 1)) value       -- Send ADA to random pub key
    void $ submitTxConstraintsSpending inst unspentOutputs tx


endpoints :: AsContractError e => Contract LotterySchema e ()
endpoints = lotterysystem theLottery

mkSchemaDefinitions ''LotterySchema

$(mkKnownCurrencies [])
