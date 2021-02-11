{-# LANGUAGE OverloadedStrings #-}
module Lottery where

import           Language.Marlowe
import           Ledger.Value
import           Data.ByteString.Internal
import           Data.String
{- 
    N Users have k slots to  deposit 1 token into the lottery role. The lottery role will select a user,
    after k slots but before k*k slots, to receive all tokens that were deposited. If the lottery does
    not select a user before k*k slots then all user are returned their 1 token deposit.

    This script can be easily changed to account for any donation/fund raiser/group purchase.
    
    The number n sets the maximum allow users to use the smart contract at one time.
    The number k sets the timeframe for the lottery to play out.

    Company: Logical Mechanism
    Author: Quinn Parkinson
    Year: 2021
-}

numUser :: Integer
numUser = 10

main :: IO ()
main = print . pretty $ genContracts (numUser, numUser)

-- A TokenName deposits 1 token to the lottery role.
buyin :: TokenName -> Action
buyin user = Deposit (Role "lottery") (Role user) (Token "" "") (Constant 1)

-- A user is paid all available tokens inside the lottery.
cashout :: TokenName -> Contract
cashout user = Pay (Role "lottery") (Party (Role user)) (Token "" "") (AvailableMoney (Role "lottery") (Token "" "")) Close

-- The lottery selects between user 1 and the nth user. One and n are in the bounds.
lotteryChoice :: Integer -> Action
lotteryChoice n = Choice (ChoiceId "choice" (Role "lottery")) [Bound 1 n]

-- The lottery can only select between 1 and n users for any value of n. If selected then cashout.
checkVote :: Integer -> Contract
checkVote 0 = Close
checkVote n =
    If (ValueEQ
            (ChoiceValue (ChoiceId "choice" (Role "lottery")))
            (Constant n))
        (cashout player)
        (checkVote (n - 1))
    where
        player = fromString ("" ++ show n)

-- If the lottery does not select a user in k*k slots then return all deposits to users.
returnDeposit :: Integer -> Contract
returnDeposit 0 = Close
returnDeposit n =
    Pay
        (Role "lottery")
        (Party (Role user))
        (Token "" "")
        (Constant 1)
        (returnDeposit (n - 1))
    where
        user = fromString ("" ++ show n)

-- Give n users chose a number between 1 and n then checVote for that selected user.
choose :: Integer -> Contract
choose 0 = Close
choose n =
    When
        [Case
            (lotteryChoice n)
            (checkVote n)
        ] time (returnDeposit n)
    where
        time = 1000

-- Recursively generate the contract for N users.
genContracts :: (Integer, Integer) -> Contract
genContracts (0, 0) = Close
genContracts (0, nmax) = choose nmax
genContracts (n, nmax) = 
    When
        [Case
            (buyin player)
            (genContracts (n - 1, nmax))
        ] time (choose (nmax - n))
    where
        player = fromString ("" ++ show (nmax - n + 1))
        time = 100
