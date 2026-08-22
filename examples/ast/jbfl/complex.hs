RuleSet
  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Just
    ( RuleSet
      { rsBySelectors = fromList
        [
          ( ObjectKey "beams", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
              ( RuleSet
                { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Just
                  ( RuleSet
                    { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                      [
                        ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                        ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsAnyArrayIndex = Just
                  ( RuleSet
                    { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Just
                      ( RuleSet
                        { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                          [
                            ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                            ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList [] } ), rsHere = fromList [], rsBelow = fromList
                  [
                    ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                    ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsHere = fromList
              [
                ( SomeKey AutoPad, SomeProperty AutoPad True ) ], rsBelow = fromList [] } ),
          ( ObjectKey "components", RuleSet
            { rsBySelectors = fromList
              [
                ( ObjectKey "electrics", RuleSet
                  { rsBySelectors = fromList
                    [
                      ( ObjectKey "smoothers", RuleSet
                        { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
                          ( RuleSet
                            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                              [
                                ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                                ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsHere = fromList [], rsBelow = fromList [] } ) ], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList [] } ) ], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList [] } ),
          ( ObjectKey "controller", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
              ( RuleSet
                { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                  [
                    ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                    ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsHere = fromList [], rsBelow = fromList [] } ),
          ( ObjectKey "flexbodies", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
              ( RuleSet
                { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Just
                  ( RuleSet
                    { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                      [
                        ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                        ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsAnyArrayIndex = Just
                  ( RuleSet
                    { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Just
                      ( RuleSet
                        { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                          [
                            ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                            ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList [] } ), rsHere = fromList [], rsBelow = fromList
                  [
                    ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                    ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsHere = fromList [], rsBelow = fromList [] } ),
          ( ObjectKey "glowMap", RuleSet
            { rsBySelectors = fromList
              [
                ( ObjectKey "abs", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                    [ ( SomeKey PadAmount, SomeProperty PadAmount 20 ) ] } ),
                ( ObjectKey "battery", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                    [ ( SomeKey PadAmount, SomeProperty PadAmount 20 ) ] } ),
                ( ObjectKey "chassis_gaugelight_warning", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList
                    [
                      ( SomeKey AlignObjectKeys, SomeProperty AlignObjectKeys True ) ], rsBelow = fromList
                    [
                      ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine Force ),
                      ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ),
                ( ObjectKey "checkengine", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                    [ ( SomeKey PadAmount, SomeProperty PadAmount 20 ) ] } ),
                ( ObjectKey "dumptruck_gaugelight_warning", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList
                    [
                      ( SomeKey AlignObjectKeys, SomeProperty AlignObjectKeys True ) ], rsBelow = fromList
                    [
                      ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine Force ),
                      ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ),
                ( ObjectKey "hazard", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                    [ ( SomeKey PadAmount, SomeProperty PadAmount 20 ) ] } ),
                ( ObjectKey "highbeam", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                    [ ( SomeKey PadAmount, SomeProperty PadAmount 20 ) ] } ),
                ( ObjectKey "lowfuel", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                    [ ( SomeKey PadAmount, SomeProperty PadAmount 20 ) ] } ),
                ( ObjectKey "lowpressure", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                    [ ( SomeKey PadAmount, SomeProperty PadAmount 20 ) ] } ),
                ( ObjectKey "parkingbrake", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                    [ ( SomeKey PadAmount, SomeProperty PadAmount 20 ) ] } ),
                ( ObjectKey "signal_L", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                    [ ( SomeKey PadAmount, SomeProperty PadAmount 20 ) ] } ),
                ( ObjectKey "signal_R", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                    [
                      ( SomeKey PadAmount, SomeProperty PadAmount 20 ) ] } ) ], rsPrefixes = [], rsAnyObjectKey = Just
              ( RuleSet
                { rsBySelectors = fromList
                  [
                    ( ObjectKey "off", RuleSet
                      { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                        [
                          ( SomeKey PadAmount, SomeProperty PadAmount 10 ) ] } ),
                    ( ObjectKey "on", RuleSet
                      { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                        [
                          ( SomeKey PadAmount, SomeProperty PadAmount 10 ) ] } ) ], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                  [
                    ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                    ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsAnyArrayIndex = Nothing, rsHere = fromList
              [
                ( SomeKey AlignObjectKeys, SomeProperty AlignObjectKeys True ),
                ( SomeKey AutoPadSubObjects, SomeProperty AutoPadSubObjects True ) ], rsBelow = fromList [] } ),
          ( ObjectKey "information", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList
              [
                ( SomeKey AlignObjectKeys, SomeProperty AlignObjectKeys True ) ], rsBelow = fromList
              [
                ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine Force ),
                ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ),
          ( ObjectKey "mainEngine", RuleSet
            { rsBySelectors = fromList
              [
                ( ObjectKey "burnEfficiency", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
                    ( RuleSet
                      { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                        [
                          ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                          ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsHere = fromList [], rsBelow = fromList [] } ),
                ( ObjectKey "torque", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
                    ( RuleSet
                      { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                        [
                          ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                          ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsHere = fromList [], rsBelow = fromList [] } ),
                ( ObjectKey "torqueModIntake", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
                    ( RuleSet
                      { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                        [
                          ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                          ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsHere = fromList [], rsBelow = fromList [] } ),
                ( ObjectKey "torqueModMult", RuleSet
                  { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
                    ( RuleSet
                      { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                        [
                          ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                          ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsHere = fromList [], rsBelow = fromList [] } ) ], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList
              [
                ( SomeKey AlignObjectKeys, SomeProperty AlignObjectKeys True ) ], rsBelow = fromList
              [
                ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine Force ),
                ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ),
          ( ObjectKey "nodes", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
              ( RuleSet
                { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Just
                  ( RuleSet
                    { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                      [
                        ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                        ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsAnyArrayIndex = Just
                  ( RuleSet
                    { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Just
                      ( RuleSet
                        { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                          [
                            ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                            ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                      [
                        ( SomeKey PadDecimals, SomeProperty PadDecimals 3 ) ] } ), rsHere = fromList [], rsBelow = fromList
                  [
                    ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ) ] } ), rsHere = fromList
              [
                ( SomeKey AlignObjectKeys, SomeProperty AlignObjectKeys True ),
                ( SomeKey AutoPad, SomeProperty AutoPad True ) ], rsBelow = fromList [] } ),
          ( ObjectKey "powertrain", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
              ( RuleSet
                { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                  [
                    ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                    ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsHere = fromList [], rsBelow = fromList [] } ),
          ( ObjectKey "props", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
              ( RuleSet
                { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Just
                  ( RuleSet
                    { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                      [
                        ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                        ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsAnyArrayIndex = Just
                  ( RuleSet
                    { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Just
                      ( RuleSet
                        { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                          [
                            ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                            ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList [] } ), rsHere = fromList [], rsBelow = fromList
                  [
                    ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                    ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsHere = fromList [], rsBelow = fromList [] } ),
          ( ObjectKey "rails", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Just
              ( RuleSet
                { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                  [
                    ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                    ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList [] } ),
          ( ObjectKey "slots", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
              ( RuleSet
                { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Just
                  ( RuleSet
                    { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                      [
                        ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                        ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsAnyArrayIndex = Just
                  ( RuleSet
                    { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Just
                      ( RuleSet
                        { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                          [
                            ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                            ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList [] } ), rsHere = fromList [], rsBelow = fromList
                  [
                    ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                    ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsHere = fromList [], rsBelow = fromList [] } ),
          ( ObjectKey "slots2", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
              ( RuleSet
                { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Just
                  ( RuleSet
                    { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                      [
                        ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                        ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsAnyArrayIndex = Just
                  ( RuleSet
                    { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Just
                      ( RuleSet
                        { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                          [
                            ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                            ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList [] } ), rsHere = fromList [], rsBelow = fromList
                  [
                    ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                    ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsHere = fromList [], rsBelow = fromList [] } ),
          ( ObjectKey "soundConfig", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList
              [
                ( SomeKey AlignObjectKeys, SomeProperty AlignObjectKeys True ) ], rsBelow = fromList
              [
                ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine Force ),
                ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ),
          ( ObjectKey "soundConfigExhaust", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList
              [
                ( SomeKey AlignObjectKeys, SomeProperty AlignObjectKeys True ) ], rsBelow = fromList
              [
                ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine Force ),
                ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ),
          ( ObjectKey "sounds", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList
              [
                ( SomeKey AlignObjectKeys, SomeProperty AlignObjectKeys True ) ], rsBelow = fromList
              [
                ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine Force ),
                ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ),
          ( ObjectKey "triangles", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
              ( RuleSet
                { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                  [
                    ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                    ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsHere = fromList
              [
                ( SomeKey AutoPad, SomeProperty AutoPad True ) ], rsBelow = fromList [] } ),
          ( ObjectKey "variables", RuleSet
            { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Just
              ( RuleSet
                { rsBySelectors = fromList [], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
                  [
                    ( SomeKey ComplexNewLine, SomeProperty ComplexNewLine None ),
                    ( SomeKey PreserveNumberFormat, SomeProperty PreserveNumberFormat True ) ] } ), rsHere = fromList [], rsBelow = fromList [] } ) ], rsPrefixes = [], rsAnyObjectKey = Nothing, rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList
        [
          ( SomeKey Indent, SomeProperty Indent 2 ),
          ( SomeKey TrailingComma, SomeProperty TrailingComma None ) ] } ), rsAnyArrayIndex = Nothing, rsHere = fromList [], rsBelow = fromList [] }
