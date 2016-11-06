module DnD exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions, onMouseEnter, onMouseLeave)
import Json.Decode as Json
import Mouse
import Debug


type alias Dragabble a b =
    Maybe
        { id : a
        , meta : b
        , position : Mouse.Position
        , atDropable : Bool
        }


type Msg a b
    = DragStart a b Mouse.Position
    | Dragging Mouse.Position
    | DragEnd Mouse.Position
    | EnterDropable
    | LeaveDropable


subscriptions : (a -> b -> Mouse.Position -> c) -> (Msg a b -> c) -> Dragabble a b -> Sub c
subscriptions onValidDrop wrap model =
    case model of
        Nothing ->
            Sub.none

        Just drag ->
            if drag.atDropable then
                Sub.batch
                    [ Mouse.moves (wrap << Dragging)
                    , Mouse.ups (onValidDrop drag.id drag.meta)
                    , Mouse.ups (wrap << DragEnd)
                    ]
            else
                Sub.batch
                    [ Mouse.moves (wrap << Dragging)
                    , Mouse.ups (wrap << DragEnd)
                    ]


update : Msg a b -> Dragabble a b -> Dragabble a b
update msg model =
    case msg of
        DragStart id meta xy ->
            Just
                { id = id
                , meta = meta
                , position = xy
                , atDropable = False
                }

        Dragging xy ->
            model
                |> Maybe.map (\d -> { d | position = xy })

        DragEnd xy ->
            Nothing

        EnterDropable ->
            model
                |> Maybe.map (\d -> { d | atDropable = True })

        LeaveDropable ->
            model
                |> Maybe.map (\d -> { d | atDropable = False })


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


dropable : (Msg a b -> c) -> a -> Html c -> Html c
dropable wrap id html =
    div
        [ onMouseEnter (wrap EnterDropable)
        , onMouseLeave (wrap LeaveDropable)
        ]
        [ html ]


px : Int -> String
px number =
    toString number ++ "px"


(=>) =
    (,)


draggedStyle position =
    style
        [ "position" => "absolute"
        , "left" => px (position.x + 10)
        , "top" => px (position.y + 10)
        ]


dragged : Dragabble a b -> (b -> Html c) -> Html c
dragged model view =
    model
        |> Maybe.map (\{ meta, position } -> div [ draggedStyle position ] [ view meta ])
        |> Maybe.withDefault (text "")
