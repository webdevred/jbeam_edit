RuleSet
    ( fromList
        [
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
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 8
                    )
                ,
                    ( SomeKey PadDecimals
                    , SomeProperty PadDecimals 3
                    )
                ]
            )
        ,
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
                    ( SomeKey NoComplexNewLine
                    , SomeProperty NoComplexNewLine True
                    )
                ]
            )
        ]
    )
