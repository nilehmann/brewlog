module IndieFlower exposing (textWidth)

import Dict exposing (Dict)


textWidth : Int -> String -> Float
textWidth fontSize text =
    List.map (charWidth fontSize) (String.toList text)
        |> List.sum


charWidth : Int -> Char -> Float
charWidth fontSize c =
    (Dict.get c mapping |> Maybe.withDefault 0.4) * toFloat fontSize


mapping : Dict Char Float
mapping =
    [ ( ' ', 0.38 )
    , ( '0', 0.43066406 )
    , ( '1', 0.31835938 )
    , ( '2', 0.4970703 )
    , ( '3', 0.43066406 )
    , ( '4', 0.41210938 )
    , ( '5', 0.57128906 )
    , ( '6', 0.4404297 )
    , ( '7', 0.41210938 )
    , ( '8', 0.375 )
    , ( '9', 0.30078125 )
    , ( '0', 0.43066406 )
    , ( 'A', 0.5625 )
    , ( 'B', 0.74121094 )
    , ( 'C', 0.515625 )
    , ( 'D', 0.74121094 )
    , ( 'E', 0.52441406 )
    , ( 'F', 0.51464844 )
    , ( 'G', 0.57128906 )
    , ( 'H', 0.52441406 )
    , ( 'I', 0.16796875 )
    , ( 'J', 0.43945313 )
    , ( 'K', 0.46875 )
    , ( 'L', 0.3466797 )
    , ( 'M', 0.5341797 )
    , ( 'N', 0.609375 )
    , ( 'O', 0.609375 )
    , ( 'P', 0.6279297 )
    , ( 'Q', 0.6425781 )
    , ( 'R', 0.57910156 )
    , ( 'S', 0.6279297 )
    , ( 'T', 0.5810547 )
    , ( 'U', 0.57128906 )
    , ( 'W', 0.703125 )
    , ( 'V', 0.421875 )
    , ( 'X', 0.5341797 )
    , ( 'Y', 0.4033203 )
    , ( 'Z', 0.57128906 )
    , ( 'a', 0.5058594 )
    , ( 'b', 0.4970703 )
    , ( 'c', 0.44921875 )
    , ( 'd', 0.52441406 )
    , ( 'e', 0.515625 )
    , ( 'f', 0.37695313 )
    , ( 'g', 0.36621094 )
    , ( 'h', 0.42773438 )
    , ( 'i', 0.17480469 )
    , ( 'j', 0.20019531 )
    , ( 'k', 0.4033203 )
    , ( 'l', 0.12011719 )
    , ( 'm', 0.75 )
    , ( 'n', 0.45996094 )
    , ( 'o', 0.4873047 )
    , ( 'p', 0.5205078 )
    , ( 'q', 0.41210938 )
    , ( 'r', 0.3466797 )
    , ( 's', 0.51464844 )
    , ( 't', 0.328125 )
    , ( 'u', 0.5019531 )
    , ( 'v', 0.38964844 )
    , ( 'w', 0.65625 )
    , ( 'x', 0.4482422 )
    , ( 'y', 0.4873047 )
    , ( 'z', 0.45996094 )
    , ( 'á', 0.5058594 )
    , ( 'é', 0.515625 )
    , ( 'í', 0.17480469 )
    , ( 'ó', 0.4873047 )
    , ( 'ú', 0.5019531 )
    , ( ',', 0.13085938 )
    , ( '.', 0.20410156 )
    , ( '/', 0.3095703 )
    , ( ';', 0.12890625 )
    , ( '’', 0.10253906 )
    , ( '\'', 0.10253906 )
    , ( '[', 0.47753906 )
    , ( ']', 0.43066406 )
    , ( '\\', 0.43066406 )
    , ( '-', 0.375 )
    , ( '=', 0.52441406 )
    , ( '<', 0.4873047 )
    , ( '>', 0.4033203 )
    , ( '?', 0.27246094 )
    , ( ':', 0.08496094 )
    , ( '"', 0.234375 )
    , ( '{', 0.3935547 )
    , ( '}', 0.4404297 )
    , ( '|', 0.14941406 )
    , ( '_', 0.65625 )
    , ( '+', 0.45996094 )
    , ( '`', 0.17285156 )
    , ( '~', 0.40234375 )
    , ( '!', 0.14941406 )
    , ( '@', 0.8154297 )
    , ( '#', 0.7685547 )
    , ( '$', 0.5810547 )
    , ( '%', 0.63671875 )
    , ( '^', 0.375 )
    , ( '&', 0.45703125 )
    , ( '*', 0.41210938 )
    , ( '(', 0.421875 )
    , ( ')', 0.29003906 )
    , ( '_', 0.65625 )
    , ( '+', 0.45996094 )
    , ( '"', 0.234375 )
    , ( ';', 0.12890625 )
    , ( '°', 0.2548828 )
    ]
        |> Dict.fromList
