{-# LANGUAGE OverloadedStrings #-}
module P2PSWAP where

import           Language.Marlowe
import           Ledger.Value
import           Data.ByteString.Internal

{-
    Marlowe Contract

    This contract models a peer-to-peer transaction with arbitary tokens with an lovelace deposit.
    Each user deposits 1 Lovelace each for the transaction fee and as a pre-agreement to the sale.
    The seller deposits the amount to be sold in their account. This can be any type of token
    on Cardano. The buyer deposits the amount to buy the seller's tokens into their account.
    Each user must agree to swap tokens after all tokens have been deposited. If both parties
    agree to swap then the sellers tokens are paid to the buyer and the buyers tokens are paid
    to the seller. The left over Lovelace is returned to both parties.

    Company: Logical Mechanism
    Author: Quinn Parkinson
    Year: 2021 
-}


main :: IO ()
main = print . pretty $ contract ("A", 100, "AA", "Token1", "B", 25, "BB", "Token2")


-- Reserves an amount of a token to a user.
reserveAmt :: (TokenName, Integer, CurrencySymbol, TokenName) -> Action
reserveAmt (user, amount, symbol, name) = Deposit (Role user) (Role user) (Token symbol name) (Constant amount)


-- UserA pays an amount of a token to userB
payment :: (TokenName, TokenName, Integer, CurrencySymbol, TokenName, Contract) -> Contract
payment (userA, userB, amount, symbol, name, cont) = Pay (Role userA) (Party (Role userB)) (Token symbol name) (Constant amount) cont


-- A binary choice, no is zero and yes is one.
yesno :: (ByteString, TokenName) -> Action
yesno (id, user) = Choice (ChoiceId id (Role user)) [Bound 0 1]


-- The p2p agreement to swap two different amounts of tokens to one another. 
agreement :: (TokenName, Integer, CurrencySymbol, TokenName,TokenName, Integer, CurrencySymbol, TokenName) -> Contract
agreement (userA, amtA, symA, nameA, userB, amtB, symB, nameB) = 
    If (ValueEQ (ChoiceValue (ChoiceId "Agreement" (Role userA))) (ChoiceValue (ChoiceId "Agreement" (Role userB)))) (  --Check Agreement
        If (ValueEQ (ChoiceValue (ChoiceId "Agreement" (Role userA))) (Constant 1)) ( --Check to trade
            payment (userA, userB, amtA, symA, nameA,  --User A Sends Token1 to User B
                payment (userB, userA, amtB, symB, nameB, --User B Sends Token2 to User A
                    Close  --End of Contract
                )
            )
        ) Close
    ) Close


-- The generic p2p contract for artbitary token swap with lovelace reservation.
contract :: (TokenName, Integer, CurrencySymbol, TokenName,TokenName, Integer, CurrencySymbol, TokenName) -> Contract
contract (userA, amtA, symA, nameA, userB, amtB, symB, nameB) = 
    When [Case (reserveAmt (userA, fee, "", "")) ( --User A Fee
        When [Case (reserveAmt (userB, fee, "", "")) (  --User A Fee
            When [Case (reserveAmt (userA, amtA, symA, nameA)) (  --User A Token1
                When [Case (reserveAmt (userB, amtB, symB, nameB)) ( --User A Token2
                    When [Case (yesno ("Agreement", userA)) (  --Do Users Agree to Trade?
                        When [Case (yesno ("Agreement", userB)) ( --Do Users Agree to Trade?
                            agreement (userA, amtA, symA, nameA, userB, amtB, symB, nameB) --If Agree trade
                        )] 60 Close
                    )] 50 Close
                )] 40 Close
            )] 30 Close
        )] 20 Close
    )] 10 Close
    where
        fee = 1

