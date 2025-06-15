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
                    , PadAmount = 6
                    )
                ,
                    ( PadDecimals
                    , PadDecimals = 3
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
                    , Selector
                        ( ArrayIndex 0 )
                    ]
                )
            , fromList
                [
                    ( PadAmount
                    , PadAmount = 8
                    )
                ]
            )
        ]
    )