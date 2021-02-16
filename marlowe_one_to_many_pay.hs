{-# LANGUAGE OverloadedStrings #-}
module Many2OnePay where

import           Language.Marlowe
import           Ledger.Value
import           Data.ByteString.Internal
import           Data.String

{-
    Marlowe Contract

    A list of users will pay another user from a list of values.

    Users: ["A", "B", "C"]
    Amnts: [123, 321, 234]

    Payee -> ["D"] = 678

    Company: Logical Mechanism
    Author: Quinn Parkinson
    Year: 2021
-}


main :: IO ()
main = print . pretty $ contract (users, amounts)


users :: [TokenName]
users = ["A", "B"]


amounts :: [Integer]
amounts = [13, 41]


-- The pot deposits a lump sum into their account.
potDeposit :: [Integer] -> Action
potDeposit amounts = 
    Deposit
        (Role "pot")
        (Role "pot")
        (Token "" "")
        (Constant (sumList amounts))


-- UserA pays an amount of a token to userB with custom contract ending.
payment :: ([TokenName], [Integer]) -> Contract
payment ([],[]) = Close
payment (user, amount) =
    Pay
        (Role "pot")
        (Party (Role (head user)))
        (Token "" "")
        (Constant (head amount))
        (payment (tail user, tail amount))


-- Get Length of a list of values.
getLength :: [a] -> Integer
getLength [] = 0
getLength arr = 1 + getLength (tail arr)


-- Get sumList
sumList :: [Integer] -> Integer
sumList [] = 0
sumList arr = head arr + sumList (tail arr)


-- Generic Contract
contract :: ([TokenName], [Integer]) -> Contract
contract (users, amounts) =
    If
    (ValueEQ
        (Constant (getLength users))
        (Constant (getLength amounts))
    )
    (When
        [Case
            (potDeposit amounts)
            (payment (users, amounts))
        ] 10 Close)
    Close
    
