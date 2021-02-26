{-# LANGUAGE OverloadedStrings #-}
module P2PSWAP where

import           Language.Marlowe
import           Ledger.Value
import           Data.ByteString.Internal
{-
    Marlowe Contract

    This contract models an questioner asking N users the answer to some question.

    Company: Logical Mechanism
    Author: Quinn Parkinson
    Year: 2021 
-}

main :: IO ()
main = print . pretty $ Close

contract :: Contract

contract = When
    [Case
        (Deposit
            (Role "owner")
            (Role "owner")
            (Token "" "")
            (Constant 10)
        )
        (When
            [Case
                (Choice
                    (ChoiceId
                        "question"
                        (Role "A")
                    )
                    [Bound 1 2]
                )
                (When
                    [Case
                        (Choice
                            (ChoiceId
                                "question"
                                (Role "B")
                            )
                            [Bound 1 2]
                        )
                        (If
                            (ValueEQ
                                (ChoiceValue
                                    (ChoiceId
                                        "question"
                                        (Role "A")
                                    ))
                                (Constant 1)
                            )
                            (Pay
                                (Role "owner")
                                (Account (Role "A"))
                                (Token "" "")
                                (Constant 1)
                                (If
                                    (ValueEQ
                                        (ChoiceValue
                                            (ChoiceId
                                                "question"
                                                (Role "B")
                                            ))
                                        (Constant 1)
                                    )
                                    (Pay
                                        (Role "owner")
                                        (Account (Role "B"))
                                        (Token "" "")
                                        (Constant 1)
                                        Close 
                                    )
                                    Close 
                                )
                            )
                            (If
                                (ValueEQ
                                    (ChoiceValue
                                        (ChoiceId
                                            "question"
                                            (Role "B")
                                        ))
                                    (Constant 1)
                                )
                                (Pay
                                    (Role "owner")
                                    (Account (Role "B"))
                                    (Token "" "")
                                    (Constant 1)
                                    Close 
                                )
                                Close 
                            )
                        )]
                    100 Close 
                )]
            100 Close 
        )]
    10 Close 