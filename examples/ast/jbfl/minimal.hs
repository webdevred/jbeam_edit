RuleSet
    ( fromList
        [
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "flexbodies" )
                    , AnyArrayIndex
                    ]
                )
            , fromList
                [
                    ( NoComplexNewLine
                    , NoComplexNewLine = True
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "nodes" )
                    , AnyArrayIndex
                    , AnyArrayIndex
                    ]
                )
            , fromList
                [
                    ( PadAmount
                    , PadAmount = 8
                    )
                ,
                    ( PadDecimals
                    , PadDecimals = 3
                    )
                ]
            )
        ]
    )