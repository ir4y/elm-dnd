module DnD
    exposing
        ( Draggable
        , Msg
        , DraggableInit
        , getDropMeta
        , getDragMeta
        , init
        , update
        , dragged
        )

{-|
This library allows you to build great UI with drag-and-drop simple.
It is abstracting you from mouse events and other low-level staff.
You can operate high-level things such as draggable and droppable areas.

The idea of package API is you should be able to wrap elements with `draggable dragMeta` to add an ability to drag it.
The dragged object will get some meta information.
Also, you could wrap another element with `droppable dropMeta`,
so if you drop element over that element, the message `OnDrop dropMeta dragMeta` will be fired.

You can find examples [here](https://github.com/ir4y/elm-dnd/tree/master/example/src).

# Draggable types and its constructor
@docs DraggableInit, Draggable, init

# Helpers to get information about draggable and droppable object
@docs getDropMeta, getDragMeta

# Message type
@docs Msg

# Update function
@docs update

# View helper
@docs dragged

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
type Draggable dropMeta dragMeta m
    = Draggable
        (Maybe
            { dragMeta : dragMeta
            , position : Mouse.Position
            , dropMeta : Maybe dropMeta
            }
        )


{-|
The type of init function result.
See `init` for more information.
-}
type alias DraggableInit dropMeta dragMeta m =
    { model : Draggable dropMeta dragMeta m
    , subscriptions : Draggable dropMeta dragMeta m -> Sub m
    , draggable : dragMeta -> List (Html.Attribute m) -> List (Html m) -> Html m
    , droppable : dropMeta -> List (Html.Attribute m) -> List (Html m) -> Html m
    }


{-|
Initialize Draggable state and function
This helper returns initModel, subscription, draggable and droppable functions
for your message wrapper.

```
type Msg
    = NoOp
    ..
    | Dropped String
    | DnDMsg (DnD.Msg String Msg)


dnd = DnD.init DnDMsg
type alias Model =
    { ...
    , draggable = dnd.model
    }
```

Subscriptions alow you to get drop event.
```
subscriptions : Model -> Sub Msg
subscriptions model =
    dnd.subscriptions model.draggable
```
View wrapper for draggable object, you could drag object wrapped by this helper
```
draggable
    : (Html.Attribute Msg)
    -> List (Html Msg)
    -> Html Msg
draggable =  dnd.draggable meta
```
View helper for the droppable area, you could drop object to this area,
after that, your `Drop meta` message will be sended.
```
droppable
  : (Html.Attribute Msg)
  -> List (Html Msg)
  -> Html Msg
droppable = dnd.droppable Dropped
```
-}
init : (Msg dropMeta dragMeta m -> m) -> (dropMeta -> dragMeta -> m) -> DraggableInit dropMeta dragMeta m
init wrap onDrop =
    { model = Draggable Nothing
    , subscriptions = subscriptions wrap onDrop
    , draggable = draggable wrap
    , droppable = droppable wrap
    }


{-|
Helper that return you a dropMeta that will be used
if an object will be dropped at the current area.
It is useful to check is it area allow you to drop an object and highlight it for example.
```
DnD.droppable Dropped
  DnDMsg
   [ style
     [ "background-color"
       => case DnD.getDropMeta model.draggable of
         Just _ ->
           "cyan"

         _ ->
           "white"
     ]
   ]
   []
```
-}
getDropMeta : Draggable dropMeta dragMeta m -> Maybe dropMeta
getDropMeta (Draggable draggable) =
    draggable
        |> Maybe.andThen .dropMeta


{-|
Helper that allow you to get meta information from a current draggable object.
You can use it to remove draggable object from the list
```
elements = model.elements
    |> List.filter
        (e -> model.draggable
            |>  getDragMeta
            |> Maybe.map (meta -> meta.id /= e.id )
        )
```
-}
getDragMeta : Draggable dropMeta dragMeta m -> Maybe dragMeta
getDragMeta (Draggable draggable) =
    draggable
        |> Maybe.map .dragMeta


{-| Inner messages, you should pass them to DnD.update at your update function.
```
type Msg
    = Dropped Item
    | DnDMsg (DnD.Msg Int Msg)
```
-}
type Msg dropMeta dragMeta m
    = DragStart dragMeta Mouse.Position
    | Dragging Mouse.Position
    | DragEnd Mouse.Position
    | EnterDroppable dropMeta
    | LeaveDroppable


subscriptions : (Msg dropMeta dragMeta m -> m) -> (dropMeta -> dragMeta -> m) -> Draggable dropMeta dragMeta m -> Sub m
subscriptions wrap onDrop (Draggable model) =
    case model of
        Nothing ->
            Sub.none

        Just drag ->
            case drag.dropMeta of
                Just dropMeta ->
                    Sub.batch
                        [ Mouse.moves (wrap << Dragging)
                        , Mouse.ups <| always <| onDrop dropMeta drag.dragMeta
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
update : Msg dropMeta dragMeta m -> Draggable dropMeta dragMeta m -> Draggable dropMeta dragMeta m
update msg (Draggable model) =
    case msg of
        DragStart dragMeta xy ->
            Draggable <|
                Just
                    { dragMeta = dragMeta
                    , position = xy
                    , dropMeta = Nothing
                    }

        Dragging xy ->
            model
                |> Maybe.map (\d -> { d | position = xy })
                |> Draggable

        DragEnd xy ->
            Draggable Nothing

        EnterDroppable dropMeta ->
            model
                |> Maybe.map (\d -> { d | dropMeta = Just dropMeta })
                |> Draggable

        LeaveDroppable ->
            model
                |> Maybe.map (\d -> { d | dropMeta = Nothing })
                |> Draggable


draggable : (Msg dropMeta dragMeta m -> m) -> dragMeta -> List (Html.Attribute m) -> List (Html m) -> Html m
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


droppable : (Msg dropMeta dragMeta m -> m) -> dropMeta -> List (Html.Attribute m) -> List (Html m) -> Html m
droppable wrap dropMeta attrs html =
    div
        (attrs
            ++ [ onMouseEnter (wrap <| EnterDroppable dropMeta)
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
View helper for a draggable object, it draws HTML of dragged object under your mouse in process of drag.
```
box : Int -> Html Msg
dragged item =
    p [] [ text item.text]

DnD.dragged
  model.draggable
  box
```
-}
dragged : Draggable dropMeta dragMeta m -> (dragMeta -> Html m) -> Html m
dragged (Draggable model) view =
    model
        |> Maybe.map (\{ dragMeta, position } -> div [ draggedStyle position ] [ view dragMeta ])
        |> Maybe.withDefault (text "")
