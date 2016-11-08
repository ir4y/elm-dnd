module DnD
    exposing
        ( Draggable
        , atDroppable
        , getMeta
        , Msg
        , subscriptions
        , update
        , draggable
        , droppable
        , dragged
        )

{-| This library allow you to simple build grate ui with drag-and-drop.
    It is astracting you from mouse events and other low level staff.
    You can operate high livel things such as draggable and droppable areas.

# Draggable type
@docs Draggable

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
    It is parametrized by the type of meta information.
-}
type alias Draggable a =
    Maybe
        { meta : a
        , position : Mouse.Position
        , atDroppable : Bool
        }


{-| Helper that allow you to check if draggable object over valid droppable area
-}
atDroppable : Draggable a -> Bool
atDroppable draggable =
    draggable
        |> Maybe.map .atDroppable
        |> Maybe.withDefault False


{-| Helper that allow you to get meta information from draggable object
-}
getMeta : Draggable a -> Maybe a
getMeta draggable =
    draggable
        |> Maybe.map .meta


{-| Inner messages, you should pass them to DnD.update at your update function
-}
type Msg a
    = DragStart a Mouse.Position
    | Dragging Mouse.Position
    | DragEnd Mouse.Position
    | EnterDroppable
    | LeaveDroppable


{-| Subscriptions alow you to get drop event
-}
subscriptions : (a -> m) -> (Msg a -> m) -> Draggable a -> Sub m
subscriptions onValidDrop wrap model =
    case model of
        Nothing ->
            Sub.none

        Just drag ->
            if drag.atDroppable then
                Sub.batch
                    [ Mouse.moves (wrap << Dragging)
                    , Mouse.ups <| always <| onValidDrop drag.meta
                    , Mouse.ups (wrap << DragEnd)
                    ]
            else
                Sub.batch
                    [ Mouse.moves (wrap << Dragging)
                    , Mouse.ups (wrap << DragEnd)
                    ]


{-| Update function handle all low level staff
-}
update : Msg a -> Draggable a -> Draggable a
update msg model =
    case msg of
        DragStart meta xy ->
            Just
                { meta = meta
                , position = xy
                , atDroppable = False
                }

        Dragging xy ->
            model
                |> Maybe.map (\d -> { d | position = xy })

        DragEnd xy ->
            Nothing

        EnterDroppable ->
            model
                |> Maybe.map (\d -> { d | atDroppable = True })

        LeaveDroppable ->
            model
                |> Maybe.map (\d -> { d | atDroppable = False })


{-| View wrapper for draggable object, you could drag object wraped by this helper
-}
draggable : (Msg a -> m) -> a -> List (Html.Attribute m) -> List (Html m) -> Html m
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
    after that your ondrop command will fire
-}
droppable : (Msg a -> m) -> List (Html.Attribute m) -> List (Html m) -> Html m
droppable wrap attrs html =
    div
        ([ onMouseEnter (wrap EnterDroppable)
         , onMouseLeave (wrap LeaveDroppable)
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


{-| View helper for draggable object, it drows html of dragged object under your mouse in process of drag
-}
dragged : Draggable a -> (a -> Html m) -> Html m
dragged model view =
    model
        |> Maybe.map (\{ meta, position } -> div [ draggedStyle position ] [ view meta ])
        |> Maybe.withDefault (text "")
