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
                    , SomeProperty NoComplexNewLine True
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
                    , SomeProperty PadAmount 8
                    )
                ,
                    ( PadDecimals
                    , SomeProperty PadDecimals 3
                    )
                ]
            )
        ]
    )
