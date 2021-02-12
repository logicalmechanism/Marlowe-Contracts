{-# LANGUAGE TypeApplications #-}
import           Control.Monad             (void)
import qualified Data.ByteString.Char8     as C
import           Language.Plutus.Contract
import qualified Language.PlutusTx         as PlutusTx
import           Language.PlutusTx.Prelude hiding (pure, (<$>))
import           Ledger                    (Address, Validator, ValidatorCtx, Value, scriptAddress, pubKeyHash)
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Playground.Contract
import qualified Prelude
import           Wallet.Emulator.Wallet    (Wallet, walletPubKey)


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
verify :: ToString -> ToString -> ValidatorCtx -> Bool
verify (ToString actual) (ToString guess) _ = actual == guess

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
    type instance DatumType LogicalDataType = ToString -- Change to any allowed Haskell Type
    type instance RedeemerType LogicalDataType = ToString -- Change to any allowed Haskell Type

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
        wrap = Scripts.wrapValidator @ToString @ToString

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
    LockParams amount password <- endpoint @"lock" @LockParams
    let tx = Constraints.mustPayToTheScript (toString password) amount
    void $ submitTxConstraints logicalInstance tx


unlock :: AsContractError e => Contract LogicalSchema e ()
unlock = do
    UnlockParams password <- endpoint @"unlock" @UnlockParams
    unspentOutputs <- utxoAt (Ledger.scriptAddress $ Scripts.validatorScript logicalInstance)
    let redeemer = toString password
        tx       = collectFromScript unspentOutputs redeemer
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
    ,   passwordLock :: String
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