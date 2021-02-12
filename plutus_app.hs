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

    Company: Logical Mechanism
    Author: Quinn Parkinson
    Year: 2021
-}


-- Validator function has the format of (Datum -> Redeemer -> ValidatorCtx -> Bool) where it is computed
-- on the chain. If we look at the ScriptInstance, this is where we define the type of the
-- Datum and Redeemer.
validateGuess :: Integer -> Integer -> ValidatorCtx -> Bool
validateGuess _ _ _ = True




-- The "lock" contract endpoint. using the LogicalSchema, we can extract the input from "lock" endpoint.
-- To submit the transaction to the network, we need (1) Script Instance (2) transaction
-- constraints, so we put restrictions on fund to be spent later by the validator
-- function, in this example the constraints will be the randomNum.
lock :: AsContractError e => Contract LogicalSchema e ()
lock = do
    LockParams amount <- endpoint @"lock" @LockParams
    let tx            = Constraints.mustPayToTheScript 123 amount
    void $ submitTxConstraints gameInstance tx

-- the script is taking two parameter DatumType which is a wrapper around the data we used
-- in the script output reedemerType is the wrapper around data we take as an input
data GameDataType
instance Scripts.ScriptType GameDataType where
    type instance DatumType GameDataType = Integer
    type instance RedeemerType GameDataType = Integer

-- this is our game instance where we take the validator script and make it part of the on-chain
-- later on we will use it as part of input of submit the transaction to the network.
gameInstance :: Scripts.ScriptInstance GameDataType
gameInstance = Scripts.validator @GameDataType
    $$(PlutusTx.compile [|| validateGuess ||])
    $$(PlutusTx.compile [|| Scripts.wrapValidator @Integer @Integer ||])


-- Each endpoint needs a parameter function of this form. The deriving
-- keywords allow Haskell to auto create functions for the endpoint.
--
-- { param1 :: String
-- , param2 :: Value
-- , param3 :: Bool
-- }
--
data LockParams = LockParams
    { amount :: Value}
    deriving stock (Prelude.Eq, Prelude.Show, Generic) -- Always include
    deriving anyclass (FromJSON, ToJSON, IotsType, ToSchema, ToArgument) -- Always include

-- The schema can consist of N endpoints. The .\/ operator combines the
-- exposed endpoints. Each endpoint has a parameter function.
--
-- .\/ Endpoint "firstOption" firstOptionParams
--
type LogicalSchema =
    BlockchainActions
        .\/ Endpoint "lock" LockParams


-- Use select to create two user inputs. This allows the app to proceed with
-- whichever option receives the input first.
--
-- endpoints firstOption `select` secondOption
--
endpoints :: AsContractError e => Contract LogicalSchema e ()
endpoints = lock


-- Bind everything to a schema definition for the application.
--
-- mkSchemaDefinitions ''exampleSchema
--
mkSchemaDefinitions ''LogicalSchema
$(mkKnownCurrencies [])