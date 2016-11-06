module DnD
    exposing
        ( Dragabble
        , atDropable
        , getMeta
        , Msg
        , subscriptions
        , update
        , dragable
        , dropable
        , dragged
        )

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions, onMouseEnter, onMouseLeave)
import Json.Decode as Json
import Mouse


type alias Dragabble a =
    Maybe
        { meta : a
        , position : Mouse.Position
        , atDropable : Bool
        }


atDropable : Dragabble a -> Bool
atDropable dragable =
    dragable
        |> Maybe.map .atDropable
        |> Maybe.withDefault False


getMeta : Dragabble a -> Maybe a
getMeta dragable =
    dragable
        |> Maybe.map .meta


type Msg a
    = DragStart a Mouse.Position
    | Dragging Mouse.Position
    | DragEnd Mouse.Position
    | EnterDropable
    | LeaveDropable


subscriptions : (a -> m) -> (Msg a -> m) -> Dragabble a -> Sub m
subscriptions onValidDrop wrap model =
    case model of
        Nothing ->
            Sub.none

        Just drag ->
            if drag.atDropable then
                Sub.batch
                    [ Mouse.moves (wrap << Dragging)
                    , Mouse.ups (\_ -> onValidDrop drag.meta)
                    , Mouse.ups (wrap << DragEnd)
                    ]
            else
                Sub.batch
                    [ Mouse.moves (wrap << Dragging)
                    , Mouse.ups (wrap << DragEnd)
                    ]


update : Msg a -> Dragabble a -> Dragabble a
update msg model =
    case msg of
        DragStart meta xy ->
            Just
                { meta = meta
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


dragable : (Msg a -> m) -> a -> List (Html.Attribute m) -> List (Html m) -> Html m
dragable wrap meta attrs html =
    div
        ([ onWithOptions "mousedown"
            { stopPropagation = True
            , preventDefault = True
            }
            (Json.map (wrap << DragStart meta) Mouse.position)
         ]
            ++ attrs
        )
        html


dropable : (Msg a -> m) -> List (Html.Attribute m) -> List (Html m) -> Html m
dropable wrap attrs html =
    div
        ([ onMouseEnter (wrap EnterDropable)
         , onMouseLeave (wrap LeaveDropable)
         ]
            ++ attrs
        )
        html


px : Int -> String
px number =
    toString number ++ "px"


(=>) =
    (,)


draggedStyle : Mouse.Position -> Html.Attribute m
draggedStyle position =
    style
        [ "position" => "absolute"
        , "left" => px (position.x + 10)
        , "top" => px (position.y + 10)
        ]


dragged : Dragabble a -> (a -> Html m) -> Html m
dragged model view =
    model
        |> Maybe.map (\{ meta, position } -> div [ draggedStyle position ] [ view meta ])
        |> Maybe.withDefault (text "")
