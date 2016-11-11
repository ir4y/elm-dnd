module DnD
    exposing
        ( Draggable
        , Msg
        , atDroppable
        , getMeta
        , init
        , subscriptions
        , update
        , draggable
        , droppable
        , dragged
        )

{-| This library allow you to simple build grate ui with drag-and-drop.
    It is astracting you from mouse events and other low level staff.
    You can operate high livel things such as draggable and droppable areas.

# Draggable type and its constructor
@docs Draggable, init

#Helper to get information about draggable object
@docs atDroppable, getMeta

#Message type
@docs Msg

#Subscriptions
@docs subscriptions

#update function
@docs update

#viwe helpers
@docs draggable, droppable, dragged

-}

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions, onMouseEnter, onMouseLeave)
import Json.Decode as Json
import Mouse


{-| Type of Draggable object it could be Nothing or Just object
    It is parametrized by the type of meta information and
    message constructor that will be invoked on Drop event
-}
type Draggable a m
    = Draggable
        (Maybe
            { meta : a
            , position : Mouse.Position
            , atDroppable : Maybe (a -> m)
            }
        )


{-| Constructor for default Draggable Model
-}
init : Draggable a m
init =
    Draggable Nothing


{-| Helper that return you a message that will be invoked
    if object will be dropped at current area
-}
atDroppable : Draggable a m -> Maybe m
atDroppable (Draggable draggable) =
    draggable
        `Maybe.andThen`
            (\d ->
                d.atDroppable
                    |> Maybe.map (\f -> f d.meta)
            )


{-| Helper that allow you to get meta information from draggable object
-}
getMeta : Draggable a m -> Maybe a
getMeta (Draggable draggable) =
    draggable
        |> Maybe.map .meta


{-| Inner messages, you should pass them to DnD.update at your update function
-}
type Msg a m
    = DragStart a Mouse.Position
    | Dragging Mouse.Position
    | DragEnd Mouse.Position
    | EnterDroppable (a -> m)
    | LeaveDroppable


{-| Subscriptions alow you to get drop event
-}
subscriptions : (Msg a m -> m) -> Draggable a m -> Sub m
subscriptions wrap (Draggable model) =
    case model of
        Nothing ->
            Sub.none

        Just drag ->
            case drag.atDroppable of
                Just onDrop ->
                    Sub.batch
                        [ Mouse.moves (wrap << Dragging)
                        , Mouse.ups <| always <| onDrop drag.meta
                        , Mouse.ups (wrap << DragEnd)
                        ]

                Nothing ->
                    Sub.batch
                        [ Mouse.moves (wrap << Dragging)
                        , Mouse.ups (wrap << DragEnd)
                        ]


{-| Update function handle all low level staff
-}
update : Msg a m -> Draggable a m -> Draggable a m
update msg (Draggable model) =
    case msg of
        DragStart meta xy ->
            Draggable <|
                Just
                    { meta = meta
                    , position = xy
                    , atDroppable = Nothing
                    }

        Dragging xy ->
            model
                |> Maybe.map (\d -> { d | position = xy })
                |> Draggable

        DragEnd xy ->
            Draggable Nothing

        EnterDroppable onValidDrop ->
            model
                |> Maybe.map (\d -> { d | atDroppable = Just onValidDrop })
                |> Draggable

        LeaveDroppable ->
            model
                |> Maybe.map (\d -> { d | atDroppable = Nothing })
                |> Draggable


{-| View wrapper for draggable object, you could drag object wraped by this helper
-}
draggable : (Msg a m -> m) -> a -> List (Html.Attribute m) -> List (Html m) -> Html m
draggable wrap meta attrs html =
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


{-| View helper for droppable area, you could drop object to this area,
    after that your on Drop message will be invoked
-}
droppable : (Msg a m -> m) -> (a -> m) -> List (Html.Attribute m) -> List (Html m) -> Html m
droppable wrap onDrop attrs html =
    div
        (attrs
            ++ [ onMouseEnter (wrap <| EnterDroppable onDrop)
               , onMouseLeave (wrap LeaveDroppable)
               ]
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


{-| View helper for draggable object, it drows html of dragged object under your mouse in process of drag
-}
dragged : Draggable a m -> (a -> Html m) -> Html m
dragged (Draggable model) view =
    model
        |> Maybe.map (\{ meta, position } -> div [ draggedStyle position ] [ view meta ])
        |> Maybe.withDefault (text "")
