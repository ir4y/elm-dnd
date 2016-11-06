module DnD exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json
import Mouse
import Debug


type alias Dragabble a b =
    Maybe
        { id : a
        , meta : b
        , position : Mouse.Position
        }


type Msg a b
    = DragStart a b Mouse.Position
    | Dragging Mouse.Position
    | DragEnd Mouse.Position


subscriptions : (Msg a b -> c) -> Dragabble a b -> Sub c
subscriptions wrap model =
    case model of
        Nothing ->
            Sub.none

        Just drag ->
            Sub.batch
                [ Mouse.moves (wrap << Dragging)
                , Mouse.ups (wrap << DragEnd)
                ]


update : Msg a b -> Dragabble a b -> Dragabble a b
update msg model =
    case Debug.log "msg" msg of
        DragStart id meta xy ->
            Just { id = id, meta = meta, position = xy }

        Dragging xy ->
            model
                |> Maybe.map (\d -> { d | position = xy })

        DragEnd xy ->
            Nothing


dragable : (Msg a b -> c) -> a -> (b -> Html c) -> b -> Html c
dragable wrap id view meta =
    div
        [ onWithOptions "mousedown"
            { stopPropagation = True
            , preventDefault = True
            }
            (Json.map (wrap << DragStart id meta) Mouse.position)
        ]
        [ view meta ]


dropable : a -> Html c -> Html c
dropable id html =
    div [] [ html ]


px : Int -> String
px number =
    toString number ++ "px"


(=>) =
    (,)


draggedStyle position =
    style
        [ "position" => "absolute"
        , "left" => px position.x
        , "top" => px position.y
        ]


dragged : Dragabble a b -> (b -> Html c) -> Html c
dragged model view =
    model
        |> Maybe.map (\{ meta, position } -> div [ draggedStyle position ] [ view meta ])
        |> Maybe.withDefault (text "")
