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
    { lotteryDeadline           = 10
    , lotteryCollectionDeadline = 50
    , lotteryPlayers            = [(player x) | x <- [2..5] ]
    , lotteryBuyin              = 100
    }

-- Creates a pubkeyhask for a wallet.
player :: Integer -> PubKeyHash
player id = (pubKeyHash $ Emulator.walletPubKey (Emulator.Wallet id))

-- RNG betwen lo and hi.
getRn :: (RandomGen g) => Integer -> Integer -> g -> (Integer, g)
getRn lo hi g = randomR (lo, hi) g

-- number of players stored in the lottery.
--
numberOfPlayers :: [a] -> Integer
numberOfPlayers [] = 0
numberOfPlayers users =  1 + (numberOfPlayers (tail users))


sumOfIncomes :: [Integer] -> Integer
sumOfIncomes [] = 0
sumOfIncomes incomes = (head incomes) + (sumOfIncomes (tail incomes))


-- | A lottery system.
data Lottery = Lottery
    { lotteryDeadline           :: Slot
    , lotteryCollectionDeadline :: Slot
    , lotteryPlayers            :: [PubKeyHash]
    , lotteryBuyin              :: Integer
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
contribute :: AsContractError e => Lottery -> Contract LotterySchema e ()
contribute cmp = do
    ()         <- endpoint @"contribute"
    let inst   = scriptInstance cmp
        tx     = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf (lotteryBuyin cmp))            -- Force 1 ADA buy in
                <> Constraints.mustValidateIn (Ledger.interval 1 (lotteryDeadline cmp)) -- Must buy in before lottery is over
    txid           <- fmap txId (submitTxConstraints inst tx)
    unspentOutputs <- utxoAt (Scripts.scriptAddress inst) 
    if Constraints.modifiesUtxoSet tx
    then void (submitTxConstraintsSpending inst unspentOutputs tx)
    else pure ()


-- | When the end slot is reached, pay a random wallet.
startLottery :: AsContractError e => Lottery -> Contract LotterySchema e ()
startLottery cmp = do
    let inst = scriptInstance cmp
    ()             <- endpoint @"start lottery"
    _              <- awaitSlot (lotteryCollectionDeadline cmp) -- wait til slot then proceed
    unspentOutputs <- utxoAt (Scripts.scriptAddress inst)       -- Get ada in script
    starter        <- pubKeyHash <$> ownPubKey                  -- the key that started the lottery
    -- The outcome can be generated here.
    let players            = numberOfPlayers (lotteryPlayers cmp)
    let incomes            = [(lotteryBuyin cmp) | x <- [0..(players-1)]]
    let outcomes           = ioGame incomes
        value              = foldMap (Validation.txOutValue . Tx.txOutTxOut . snd) (Map.toList unspentOutputs) -- Get value in script
        tx                 = Typed.collectFromScript unspentOutputs ()
                            <> (createTX (lotteryPlayers cmp) outcomes starter)
    -- logInfo @[Value] outcomes
    void $ submitTxConstraintsSpending inst unspentOutputs tx


-- The starter of the game must be paid their 1 ada back but this allows every player to be paid uniquely.
createTX :: [PubKeyHash] -> [Integer] -> PubKeyHash -> Constraints.TxConstraints () ()
createTX [] values starter = Constraints.mustPayToPubKey starter (Ada.toValue 1)
createTX players values starter = (Constraints.mustPayToPubKey (head players) (Ada.lovelaceValueOf (head values)))
                                    <> (createTX (tail players) (tail values) starter)


ioGame :: [Integer] -> [Integer]
ioGame incomes = do
    let players  = numberOfPlayers (incomes)
        total    = sumOfIncomes (incomes)
        miners   = [0 | x <- [0..(players-1)]]
        gen      = mkStdGen 123
        outcomes = mining total miners gen
    outcomes


mining :: Integer -> [Integer] -> StdGen -> [Integer]
mining total [] gen = []
mining 0 miners gen = miners
mining total miners gen = do
    let (mineRoll, nextGen) = getRn 0 100 gen
    let (rewardRoll, nextGen2) = getRn 1 10 nextGen
    if mineRoll > 86
    then (mining (total - rewardRoll) ((tail miners) ++ [((head miners)+rewardRoll)]) nextGen2)
    else (mining (total) ((tail miners) ++ [((head miners))]) nextGen)


endpoints :: AsContractError e => Contract LotterySchema e ()
endpoints = lotterysystem theLottery


mkSchemaDefinitions ''LotterySchema


$(mkKnownCurrencies [])
