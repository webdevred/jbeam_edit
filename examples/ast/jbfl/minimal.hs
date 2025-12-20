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
                    , AnyArrayIndex
                    , AnyArrayIndex
                    ]
                )
            , fromList
                [
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
                        ( ObjectKey "beams" )
                    , AnyArrayIndex
                    ]
                )
            , fromList
                [
                    ( SomeKey ComplexNewLine
                    , SomeProperty ComplexNewLine None
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
                    ( SomeKey ComplexNewLine
                    , SomeProperty ComplexNewLine None
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
                    ( SomeKey ComplexNewLine
                    , SomeProperty ComplexNewLine None
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
                    ( SomeKey ComplexNewLine
                    , SomeProperty ComplexNewLine None
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "props" )
                    , AnyArrayIndex
                    ]
                )
            , fromList
                [
                    ( SomeKey ComplexNewLine
                    , SomeProperty ComplexNewLine None
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "rails" )
                    , AnyObjectKey
                    ]
                )
            , fromList
                [
                    ( SomeKey ComplexNewLine
                    , SomeProperty ComplexNewLine None
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
                    ( SomeKey ComplexNewLine
                    , SomeProperty ComplexNewLine Force
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "triangles" )
                    , AnyArrayIndex
                    ]
                )
            , fromList
                [
                    ( SomeKey ComplexNewLine
                    , SomeProperty ComplexNewLine None
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
                    ( SomeKey ComplexNewLine
                    , SomeProperty ComplexNewLine None
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
                    ( SomeKey ComplexNewLine
                    , SomeProperty ComplexNewLine Force
                    )
                ]
            )
        ]
    )
