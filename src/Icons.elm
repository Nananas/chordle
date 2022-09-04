module Icons exposing (..)

import Element exposing (..)
import Element.Events
import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes


menu size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.path
                    [ Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.d "M4 6h16M4 12h16M4 18h16"
                    ]
                    []
                ]


questionmark size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.path
                    [ Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.d "M8.228 9c.549-1.165 2.03-2 3.772-2 2.21 0 4 1.343 4 3 0 1.4-1.278 2.575-3.006 2.907-.542.104-.994.54-.994 1.093m0 3h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
                    ]
                    []
                ]


keyboard size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "0 0 576 512"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ {- ! Font Awesome Pro 6.1.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license (Commercial License) Copyright 2022 Fonticons, Inc. -}
                  Svg.path
                    [ Svg.Attributes.d "M512 448H64c-35.35 0-64-28.65-64-64V128c0-35.35 28.65-64 64-64h448c35.35 0 64 28.65 64 64v256C576 419.3 547.3 448 512 448zM128 180v-40C128 133.4 122.6 128 116 128h-40C69.38 128 64 133.4 64 140v40C64 186.6 69.38 192 76 192h40C122.6 192 128 186.6 128 180zM224 180v-40C224 133.4 218.6 128 212 128h-40C165.4 128 160 133.4 160 140v40C160 186.6 165.4 192 172 192h40C218.6 192 224 186.6 224 180zM320 180v-40C320 133.4 314.6 128 308 128h-40C261.4 128 256 133.4 256 140v40C256 186.6 261.4 192 268 192h40C314.6 192 320 186.6 320 180zM416 180v-40C416 133.4 410.6 128 404 128h-40C357.4 128 352 133.4 352 140v40C352 186.6 357.4 192 364 192h40C410.6 192 416 186.6 416 180zM512 180v-40C512 133.4 506.6 128 500 128h-40C453.4 128 448 133.4 448 140v40C448 186.6 453.4 192 460 192h40C506.6 192 512 186.6 512 180zM128 276v-40C128 229.4 122.6 224 116 224h-40C69.38 224 64 229.4 64 236v40C64 282.6 69.38 288 76 288h40C122.6 288 128 282.6 128 276zM224 276v-40C224 229.4 218.6 224 212 224h-40C165.4 224 160 229.4 160 236v40C160 282.6 165.4 288 172 288h40C218.6 288 224 282.6 224 276zM320 276v-40C320 229.4 314.6 224 308 224h-40C261.4 224 256 229.4 256 236v40C256 282.6 261.4 288 268 288h40C314.6 288 320 282.6 320 276zM416 276v-40C416 229.4 410.6 224 404 224h-40C357.4 224 352 229.4 352 236v40C352 282.6 357.4 288 364 288h40C410.6 288 416 282.6 416 276zM512 276v-40C512 229.4 506.6 224 500 224h-40C453.4 224 448 229.4 448 236v40C448 282.6 453.4 288 460 288h40C506.6 288 512 282.6 512 276zM128 372v-40C128 325.4 122.6 320 116 320h-40C69.38 320 64 325.4 64 332v40C64 378.6 69.38 384 76 384h40C122.6 384 128 378.6 128 372zM416 372v-40C416 325.4 410.6 320 404 320h-232C165.4 320 160 325.4 160 332v40C160 378.6 165.4 384 172 384h232C410.6 384 416 378.6 416 372zM512 372v-40C512 325.4 506.6 320 500 320h-40C453.4 320 448 325.4 448 332v40C448 378.6 453.4 384 460 384h40C506.6 384 512 378.6 512 372z"
                    ]
                    []
                ]


bell size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.path
                    [ Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.d "M15 17h5l-1.405-1.405A2.032 2.032 0 0118 14.158V11a6.002 6.002 0 00-4-5.659V5a2 2 0 10-4 0v.341C7.67 6.165 6 8.388 6 11v3.159c0 .538-.214 1.055-.595 1.436L4 17h5m6 0v1a3 3 0 11-6 0v-1m6 0H9"
                    ]
                    []
                ]


refresh size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.path
                    [ Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.d "M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"
                    ]
                    []
                ]


book size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.path
                    [ Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.d "M12 6.253v13m0-13C10.832 5.477 9.246 5 7.5 5S4.168 5.477 3 6.253v13C4.168 18.477 5.754 18 7.5 18s3.332.477 4.5 1.253m0-13C13.168 5.477 14.754 5 16.5 5c1.747 0 3.332.477 4.5 1.253v13C19.832 18.477 18.247 18 16.5 18c-1.746 0-3.332.477-4.5 1.253"
                    ]
                    []
                ]


academicCap size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.path
                    [ Svg.Attributes.d "M12 14l9-5-9-5-9 5 9 5z"
                    ]
                    []
                , Svg.path [ Svg.Attributes.d "M12 14l6.16-3.422a12.083 12.083 0 01.665 6.479A11.952 11.952 0 0012 20.055a11.952 11.952 0 00-6.824-2.998 12.078 12.078 0 01.665-6.479L12 14z" ] []
                , Svg.path [ Svg.Attributes.strokeLinecap "round", Svg.Attributes.strokeLinejoin "round", Svg.Attributes.d "M12 14l9-5-9-5-9 5 9 5zm0 0l6.16-3.422a12.083 12.083 0 01.665 6.479A11.952 11.952 0 0012 20.055a11.952 11.952 0 00-6.824-2.998 12.078 12.078 0 01.665-6.479L12 14zm-4 6v-7.5l4-2.222" ] []
                ]


translate size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.path
                    [ Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.d "M3 5h12M9 3v2m1.048 9.5A18.022 18.022 0 016.412 9m6.088 9h7M11 21l5-10 5 10M12.751 5C11.783 10.77 8.07 15.61 3 18.129"
                    ]
                    []
                ]


eye size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.path
                    [ Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.d "M15 12a3 3 0 11-6 0 3 3 0 016 0z"
                    ]
                    []
                , Svg.path [ Svg.Attributes.strokeLinecap "round", Svg.Attributes.strokeLinejoin "round", Svg.Attributes.d "M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z" ] []
                ]


eyeOff size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.path
                    [ Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.d "M13.875 18.825A10.05 10.05 0 0112 19c-4.478 0-8.268-2.943-9.543-7a9.97 9.97 0 011.563-3.029m5.858.908a3 3 0 114.243 4.243M9.878 9.878l4.242 4.242M9.88 9.88l-3.29-3.29m7.532 7.532l3.29 3.29M3 3l3.59 3.59m0 0A9.953 9.953 0 0112 5c4.478 0 8.268 2.943 9.543 7a10.025 10.025 0 01-4.132 5.411m0 0L21 21"
                    ]
                    []
                ]


firstTone size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "0 0 26 26"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.g []
                    [ Svg.path
                        [ Svg.Attributes.d "m3 13h20"
                        , Svg.Attributes.strokeLinecap "round"
                        , Svg.Attributes.strokeWidth "4"
                        ]
                        []
                    ]
                ]


secondTone size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "0 0 26 26"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.g []
                    [ Svg.path
                        [ Svg.Attributes.d "m5 18 17-9"
                        , Svg.Attributes.strokeLinecap "round"
                        , Svg.Attributes.strokeWidth "4"
                        ]
                        []
                    ]
                ]


thirdTone size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "0 0 26 26"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.g []
                    [ Svg.path
                        [ Svg.Attributes.d "m5 14c9 10 10 1 17-10"
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.strokeLinecap "round"
                        , Svg.Attributes.strokeWidth "4"
                        ]
                        []
                    ]
                ]


forthTone size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "0 0 26 26"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.g []
                    [ Svg.path
                        [ Svg.Attributes.d "m5 6 17 13"
                        , Svg.Attributes.strokeLinecap "round"
                        , Svg.Attributes.strokeWidth "4"
                        ]
                        []
                    ]
                ]


fifthTone size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "0 0 26 26"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.g []
                    [ Svg.path
                        [ Svg.Attributes.d "m5 14h2"
                        , Svg.Attributes.strokeLinecap "round"
                        , Svg.Attributes.strokeWidth "4"
                        ]
                        []
                    ]
                ]


github size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.width "43.43866"
                , Svg.Attributes.height "42.366486"
                , Svg.Attributes.viewBox "0 0 43.43866 42.366486"
                ]
                [ Svg.g
                    [ Svg.Attributes.transform "matrix(1.3333333,0,0,-1.3333333,-181.76,592.72665)"
                    ]
                    [ Svg.g
                        [ Svg.Attributes.transform "scale(0.1)"
                        ]
                        [ Svg.path
                            [ Svg.Attributes.d "M 1526.08,4445.45 C 1436.14,4445.45 1363.2,4372.52 1363.2,4282.55 1363.2,4210.59 1409.87,4149.53 1474.6,4127.98 1482.75,4126.49 1485.72,4131.52 1485.72,4135.84 1485.72,4139.71 1485.58,4149.95 1485.5,4163.54 1440.19,4153.7 1430.63,4185.38 1430.63,4185.38 1423.22,4204.2 1412.54,4209.21 1412.54,4209.21 1397.75,4219.31 1413.66,4219.11 1413.66,4219.11 1430.01,4217.96 1438.61,4202.32 1438.61,4202.32 1453.14,4177.43 1476.74,4184.62 1486.02,4188.79 1487.5,4199.31 1491.71,4206.49 1496.36,4210.56 1460.19,4214.68 1422.16,4228.65 1422.16,4291.07 1422.16,4308.86 1428.51,4323.39 1438.93,4334.78 1437.25,4338.9 1431.66,4355.46 1440.53,4377.89 1440.53,4377.89 1454.2,4382.27 1485.32,4361.2 1498.31,4364.81 1512.25,4366.61 1526.1,4366.68 1539.94,4366.61 1553.87,4364.81 1566.88,4361.2 1597.98,4382.27 1611.63,4377.89 1611.63,4377.89 1620.52,4355.46 1614.93,4338.9 1613.26,4334.78 1623.7,4323.39 1630,4308.86 1630,4291.07 1630,4228.49 1591.91,4214.72 1555.63,4210.69 1561.47,4205.66 1566.68,4195.72 1566.68,4180.52 1566.68,4158.75 1566.48,4141.18 1566.48,4135.84 1566.48,4131.48 1569.42,4126.41 1577.68,4128 1642.36,4149.59 1688.99,4210.6 1688.99,4282.55 1688.99,4372.52 1616.05,4445.45 1526.08,4445.45"
                            , Svg.Attributes.fill "currentColor"
                            ]
                            []
                        ]
                    ]
                ]
