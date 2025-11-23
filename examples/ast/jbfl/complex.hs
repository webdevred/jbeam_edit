RuleSet
    ( fromList
        [
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "glowMap" )
                    , AnyObjectKey
                    , Selector
                        ( ObjectKey "off" )
                    ]
                )
            , fromList
                [
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 10
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "glowMap" )
                    , AnyObjectKey
                    , Selector
                        ( ObjectKey "on" )
                    ]
                )
            , fromList
                [
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 10
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "nodes" )
                    , Selector
                        ( ArrayIndex 0 )
                    , AnyArrayIndex
                    ]
                )
            , fromList
                [
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 0
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
                        ( ObjectKey "glowMap" )
                    , Selector
                        ( ObjectKey "abs" )
                    ]
                )
            , fromList
                [
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 20
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "glowMap" )
                    , Selector
                        ( ObjectKey "battery" )
                    ]
                )
            , fromList
                [
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 20
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "glowMap" )
                    , Selector
                        ( ObjectKey "checkengine" )
                    ]
                )
            , fromList
                [
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 20
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "glowMap" )
                    , Selector
                        ( ObjectKey "hazard" )
                    ]
                )
            , fromList
                [
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 20
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "glowMap" )
                    , Selector
                        ( ObjectKey "highbeam" )
                    ]
                )
            , fromList
                [
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 20
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "glowMap" )
                    , Selector
                        ( ObjectKey "lowfuel" )
                    ]
                )
            , fromList
                [
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 20
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "glowMap" )
                    , Selector
                        ( ObjectKey "lowpressure" )
                    ]
                )
            , fromList
                [
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 20
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "glowMap" )
                    , Selector
                        ( ObjectKey "parkingbrake" )
                    ]
                )
            , fromList
                [
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 20
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "glowMap" )
                    , Selector
                        ( ObjectKey "signal_L" )
                    ]
                )
            , fromList
                [
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 20
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "glowMap" )
                    , Selector
                        ( ObjectKey "signal_R" )
                    ]
                )
            , fromList
                [
                    ( SomeKey PadAmount
                    , SomeProperty PadAmount 20
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "glowMap" )
                    , AnyObjectKey
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
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "slots" )
                    , AnyArrayIndex
                    ]
                )
            , fromList
                [
                    ( SomeKey ForceComplexNewLine
                    , SomeProperty ForceComplexNewLine True
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "variables" )
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
                        ( ObjectKey "sounds" )
                    ]
                )
            , fromList
                [
                    ( SomeKey ForceComplexNewLine
                    , SomeProperty ForceComplexNewLine True
                    )
                ]
            )
        ]
    )
