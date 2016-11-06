module DnD exposing (..)

import Html exposing (..)


type alias Dragabble a b =
    Maybe
        { id : a
        , meta : b
        }


type Msg
    = NoOp


subscriptions model =
    Sub.none


update : Msg -> Dragabble a b -> Dragabble a b
update msg model =
    model


dragable : a -> Html c -> Html c
dragable id html =
    div [] [ html ]


dropable : a -> Html c -> Html c
dropable id html =
    div [] [ html ]


dragged : Dragabble a b -> (b -> Html c) -> Html c
dragged model view =
    model
        |> Maybe.map (\{ meta } -> div [] [ view meta ])
        |> Maybe.withDefault (text "")
