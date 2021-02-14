{-# LANGUAGE TypeApplications #-}
import           Control.Applicative       (Applicative (pure))
import           Control.Monad             (void)
import qualified Data.ByteString.Char8     as C
import qualified Prelude
import           Language.Plutus.Contract
import qualified Language.PlutusTx         as PlutusTx
import           Language.PlutusTx.Prelude hiding (pure, (<$>), Applicative (..), Semigroup (..))
import qualified Language.Plutus.Contract.Typed.Tx    as Typed
import           Ledger                    (Address, Validator, ValidatorCtx, Value, scriptAddress,PubKeyHash, pubKeyHash, TxInfo (..), Validator, ValidatorCtx (..))
import qualified Ledger                               as Ledger
import qualified Ledger.Ada                           as Ada
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Contexts                      as V
import qualified Ledger.Interval                      as Interval
import qualified Ledger.Scripts                       as Scripts
import           Ledger.Slot                          (Slot, SlotRange)
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Ledger.Value                         as Value
import           Playground.Contract
import           Wallet.Emulator.Wallet    (Wallet, walletPubKey)
import qualified Wallet.Emulator                      as Emulator
import           Control.Applicative                  (Applicative (pure))
import           Control.Monad                        (void)
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
import           Playground.Contract
import           Prelude                              (Semigroup (..))
import qualified Prelude                              as Haskell
import qualified Wallet.Emulator                      as Emulator
{-
    Plutus Contract

    Each exposed endpoint uses the same instance and validator.

    Company: Logical Mechanism
    Author: Quinn Parkinson
    Year: 2021
-}

-------------------------------------------------------------------------------

-- Custom DataTypes lifted into Plutus from Haskell. This allows the validator
-- function to have custom datatypes has its input and output. Each newtype 
-- is used to wrap the value.
--
-- Reference 
--
newtype ToString = ToString ByteString deriving newtype PlutusTx.IsData
PlutusTx.makeLift ''ToString
toString :: String -> ToString
toString = ToString . C.pack

-------------------------------------------------------------------------------

-- The Validator function has the format:
--
-- (Datum -> Redeemer -> ValidatorCtx -> Bool).
--
-- This is computed on the chain.
-- The Datum and Redeemer types are defined in the exampleInstance data type ExampleDataType.
-- 
--  This function just passes True.
--
-- @see: LogicalDataType
--
verify :: () -> () -> ValidatorCtx -> Bool
verify _ _ _ = True

-------------------------------------------------------------------------------

-- The DataType describes the type of values used in the Datum and Redeemer.
-- These two parameters are wrappers around the data we use for the input and output.
--
-- data ExampleDataType
--
-- The format is standard for each validator.
-- 
data LogicalDataType
instance Scripts.ScriptType LogicalDataType where
    type instance DatumType LogicalDataType = () -- Change to any allowed Haskell Type
    type instance RedeemerType LogicalDataType = () -- Change to any allowed Haskell Type

-------------------------------------------------------------------------------

-- The script instance contains the information about the validator script.
-- This allows the input to be submitted to the chain. Every validator has
-- the same form:
--
-- (Datum -> Redeemer -> ValidatorCtx -> Bool)
--
-- The Datum and Redeemer types are described in a data object. This allows
-- a lot of creativity for input types for each endpoint. The types need
-- to be declared inside the instance for each transaction.
--
-- This function describes the type of validator used to validate a tx.
-- 
-- @see: LogicalDataType
-- @see: verify
--
logicalInstance :: Scripts.ScriptInstance LogicalDataType
logicalInstance = Scripts.validator @LogicalDataType
    $$(PlutusTx.compile [|| verify ||]) -- input validator function name here
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @() @()

-------------------------------------------------------------------------------

-- The contract endpoints.
-- The contract is a transaction to the blockchain.
--
-- exampleParams var1 var2 var3 <- endpoint @"example" @exampleParams
-- let tx = Contraints.mustPayToTheScript uniqueID amount
-- void $ submitTxConstraints exampleInstance tx
--
-- Submitting the tx constraints needs to run through a validator for the chain.
-- In this case, logicalInstance is the container for the validator.
--
-- @see: logicalInstance
--
lock :: AsContractError e => Contract LogicalSchema e ()
lock = do
    LockParams amount <- endpoint @"lock" @LockParams
    -- contributor <- pubKeyHash <$> ownPubKey
    let tx = Constraints.mustPayToTheScript () amount
    void $ submitTxConstraints logicalInstance tx


unlock :: AsContractError e => Contract LogicalSchema e ()
unlock = do
    UnlockParams password <- endpoint @"unlock" @UnlockParams
    
    unspentOutputs <- utxoAt (Ledger.scriptAddress $ Scripts.validatorScript logicalInstance)
    -- winner         <- Ledger.pubKeyHash ((Emulator.walletPubKey (Emulator.Wallet 2)))
    winner         <- pubKeyHash <$> ownPubKey
    let tx = Typed.collectFromScript unspentOutputs ()
            <> Constraints.mustPayToPubKey winner (Ada.toValue 2)
    void $ submitTxConstraintsSpending logicalInstance unspentOutputs tx -- Much be a different wallet than the wallet that submited the lock.


-------------------------------------------------------------------------------

-- Each endpoint needs a parameter function of this form. The deriving
-- keywords allow Haskell to auto create functions for the endpoint.
--
-- { param1 :: String
-- , param2 :: Value
-- , param3 :: Bool
-- , param4 :: Integer
-- }
--
-- A Value is assumed to be a Lovelace and an Integer is a number.
--
-- The data keyword is lifted - that is, they contain their own ⊥ value that
-- is distinct from all the others. The mathematical symbol for bottom is '⊥'
-- and it refers to a computation which never completes successfully.
--
-- But it can also be a newtype.
--
data LockParams = LockParams
    {
        amount   :: Value
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic) -- Always include
    deriving anyclass (FromJSON, ToJSON, IotsType, ToSchema, ToArgument) -- Always include


data UnlockParams = UnlockParams
    { passwordUnlock :: String}
    deriving stock (Prelude.Eq, Prelude.Show, Generic) -- Always include
    deriving anyclass (FromJSON, ToJSON, IotsType, ToSchema, ToArgument) -- Always include


-- The schema can consist of N endpoints. The .\/ operator combines the
-- exposed endpoints. Each endpoint has a parameter function.
--
-- .\/ Endpoint "exampleOption" exampleOptionParams
--
-- @see: lock, unlock
-- @see: LockParams, UnlockParams
--
type LogicalSchema =
    BlockchainActions
        .\/ Endpoint "lock" LockParams
        .\/ Endpoint "unlock" UnlockParams


-- Use select to create two user inputs. This allows the app to proceed with
-- whichever option receives the input first.
--
-- endpoints exampleOption `select` secondOption
--
-- @see: lock
-- @see: unlock
--
endpoints :: AsContractError e => Contract LogicalSchema e ()
endpoints = lock `select` unlock


-- Bind everything to a schema definition for the application.
--
-- mkSchemaDefinitions ''exampleSchema
--
-- @see: LogicalSchema
--
mkSchemaDefinitions ''LogicalSchema
$(mkKnownCurrencies [])