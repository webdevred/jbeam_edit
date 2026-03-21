RuleSet
    ( fromList
        [
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "components" )
                    , Selector
                        ( ObjectKey "electrics" )
                    , Selector
                        ( ObjectKey "smoothers" )
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
                        ( ObjectKey "mainEngine" )
                    , Selector
                        ( ObjectKey "burnEfficiency" )
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
                        ( ObjectKey "mainEngine" )
                    , Selector
                        ( ObjectKey "torque" )
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
                        ( ObjectKey "controller" )
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
                        ( ObjectKey "powertrain" )
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
                        ( ObjectKey "props" )
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
                        ( ObjectKey "rails" )
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
                        ( ObjectKey "slots" )
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
                        ( ObjectKey "slots2" )
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
                        ( ObjectKey "triangles" )
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
                        ( ObjectKey "beams" )
                    ]
                )
            , fromList
                [
                    ( SomeKey AutoPad
                    , SomeProperty AutoPad True
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "glowMap" )
                    ]
                )
            , fromList
                [
                    ( SomeKey AlignObjectKeys
                    , SomeProperty AlignObjectKeys True
                    )
                ,
                    ( SomeKey AutoPadSubObjects
                    , SomeProperty AutoPadSubObjects True
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "information" )
                    ]
                )
            , fromList
                [
                    ( SomeKey AlignObjectKeys
                    , SomeProperty AlignObjectKeys True
                    )
                ,
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
                        ( ObjectKey "nodes" )
                    ]
                )
            , fromList
                [
                    ( SomeKey AlignObjectKeys
                    , SomeProperty AlignObjectKeys True
                    )
                ,
                    ( SomeKey AutoPad
                    , SomeProperty AutoPad True
                    )
                ]
            )
        ,
            ( NodePattern
                ( fromList
                    [ AnyObjectKey
                    , Selector
                        ( ObjectKey "soundConfig" )
                    ]
                )
            , fromList
                [
                    ( SomeKey AlignObjectKeys
                    , SomeProperty AlignObjectKeys True
                    )
                ,
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
                        ( ObjectKey "soundConfigExhaust" )
                    ]
                )
            , fromList
                [
                    ( SomeKey AlignObjectKeys
                    , SomeProperty AlignObjectKeys True
                    )
                ,
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
                        ( ObjectKey "sounds" )
                    ]
                )
            , fromList
                [
                    ( SomeKey AlignObjectKeys
                    , SomeProperty AlignObjectKeys True
                    )
                ,
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
                        ( ObjectKey "triangles" )
                    ]
                )
            , fromList
                [
                    ( SomeKey AutoPad
                    , SomeProperty AutoPad True
                    )
                ]
            )
        ]
    )
