RuleSet
    ( fromList
        [
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
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 8
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
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 6
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
                    ( SomeKey NoComplexNewLine
                    , SomeProperty NoComplexNewLine True
                    )
                ]
            )
        ]
    )
