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

{-|
This library allows you to build great UI with drag-and-drop simple.
It is abstracting you from mouse events and other low-level staff.
You can operate high-level things such as draggable and droppable areas.

The idea of package API is you should be able to wrap elements with `draggable meta` to add an ability to drag it.
The dragget object will get some meta information.
Also, you could wrap another element with `droppable OnDrop`,
so if you drop element over that element, the message `OnDrop meta` will be fired.

Actually, we need a message wrapper so the actual signature will be
`droppable OnDrop DnDMsg` and `OnDrop meta DnDMsg`.
this functions will generate a view function which will render `div` tag with handlers for drag and drop mouse events.

So finally API will be
```
droppable
    : (Html.Attribute Msg)
    -> List (Html Msg)
    -> Html Msg
droppable = DnD.droppable Dropped DnDMsg

draggable
    : (Html.Attribute Msg)
    -> List (Html Msg)
    -> Html Msg
draggable =  DnD.draggable meta DnDMsg


view = div []
    [ draggable [class "drag-me"] ["drag me please"]
    , droppable [class "drop-here"] []
    ]
```
You can find examples [here](https://github.com/ir4y/elm-dnd/tree/master/example/src).

# Draggable type and its constructor
@docs Draggable, init

# Helpers to get information about draggable object
@docs atDroppable, getMeta

# Message type
@docs Msg

# Subscriptions
@docs subscriptions

# Update function
@docs update

# View helpers
@docs draggable, droppable, dragged

-}

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions, onMouseEnter, onMouseLeave)
import Json.Decode as Json
import Mouse


{-|
Type of Draggable object.
It is parametrized by the type of meta information and
message constructor that will be invoked on Drop event
You should place it inside your Model.
```
type alias Model =
    { draggable : DnD.Draggable Int Msg
    , count : Int
    }
```
-}
type Draggable a m
    = Draggable
        (Maybe
            { meta : a
            , position : Mouse.Position
            , atDroppable : Maybe (a -> m)
            }
        )


{-|
Constructor for Draggable to use inside your init method or where you else want.
```
init : ( Model, Cmd Msg )
init =
    ( Model DnD.init 0, Cmd.none )
```
-}
init : Draggable a m
init =
    Draggable Nothing


{-|
Helper that return you a message that will be invoked.
if an object will be dropped at the current area.
It is useful to check is it area allow you to drop object and highlight it for example.
```
DnD.droppable Dropped
  DnDMsg
   [ style
     [ "background-color"
       => case DnD.atDroppable model.draggable of
         Just (Dropped _) ->
           "cyan"

         _ ->
           "white"
     ]
   ]
   []
```
-}
atDroppable : Draggable a m -> Maybe m
atDroppable (Draggable draggable) =
    draggable
        `Maybe.andThen`
            (\d ->
                d.atDroppable
                    |> Maybe.map (\f -> f d.meta)
            )


{-|
Helper that allow you to get meta information from current draggable object.
You can use it to remove draggable object from the list
```
elements = model.elements
    |> List.filter
        (e -> model.draggable
            |>  getMeta
            |> Maybe.map (meta -> meta.id /= e.id )
        )
```
-}
getMeta : Draggable a m -> Maybe a
getMeta (Draggable draggable) =
    draggable
        |> Maybe.map .meta


{-| Inner messages, you should pass them to DnD.update at your update function.
```
type Msg
    = Dropped Item
    | DnDMsg (DnD.Msg Int Msg)
```
-}
type Msg a m
    = DragStart a Mouse.Position
    | Dragging Mouse.Position
    | DragEnd Mouse.Position
    | EnterDroppable (a -> m)
    | LeaveDroppable


{-|
Subscriptions alow you to get drop event.
```
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ DnD.subscriptions DnDMsg model.draggable
        ]
```
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


{-| Update function handle all low-level staff
```
update : Msg -> Model -> Model
update msg model =
    case msg of
        Dropped item ->
            { model | count = item + 1 }

        DnDMsg msg ->
            { model
                | draggable
                    = DnD.update msg model.draggable }
``
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


{-|
View wrapper for draggable object, you could drag object wraped by this helper
```
draggable
    : (Html.Attribute Msg)
    -> List (Html Msg)
    -> Html Msg
draggable =  DnD.draggable meta DnDMsg


draggable [class "drag-me"] ["drag me please"]
```
-}
draggable : a -> (Msg a m -> m) -> List (Html.Attribute m) -> List (Html m) -> Html m
draggable meta wrap attrs html =
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


{-|
View helper for droppable area, you could drop object to this area,
after that your on `Drop meta` message will be invoked.
```
droppable
  : (Html.Attribute Msg)
  -> List (Html Msg)
  -> Html Msg
droppable = DnD.droppable Dropped DnDMsg


droppable [class "drop-here"] []
```
-}
droppable : (a -> m) -> (Msg a m -> m) -> List (Html.Attribute m) -> List (Html m) -> Html m
droppable onDrop wrap attrs html =
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


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


draggedStyle : Mouse.Position -> Html.Attribute m
draggedStyle position =
    style
        [ "position" => "absolute"
        , "left" => px (position.x + 10)
        , "top" => px (position.y + 10)
        ]


{-|
View helper for draggable object, it drows html of dragged object under your mouse in process of drag.
```
box : Int -> Html Msg
dragged item =
    p [] [ text item.text]

DnD.dragged
  model.draggable
  box
```
-}
dragged : Draggable a m -> (a -> Html m) -> Html m
dragged (Draggable model) view =
    model
        |> Maybe.map (\{ meta, position } -> div [ draggedStyle position ] [ view meta ])
        |> Maybe.withDefault (text "")
