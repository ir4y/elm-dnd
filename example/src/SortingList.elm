module SortingList exposing (Draggable, DraggableMsg, Item, Model, Msg(..), box, dnd, droppable, init, initList, main, move, moveLeft, moveRight, subscriptions, update, update_, view)

import Browser
import DnD
import Html exposing (..)
import Html.Attributes exposing (style)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


dnd =
    DnD.init DnDMsg Dropped


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dnd.subscriptions model.draggable
        ]


type alias Draggable =
    DnD.Draggable Int ( Int, String )


type alias DraggableMsg =
    DnD.Msg Int ( Int, String )


type alias Item =
    { value : Int
    , repr : String
    }


type alias Model =
    { draggable : Draggable
    , items : List Item
    }


initList : List Item
initList =
    [ Item 1 "one"
    , Item -1 "- one"
    , Item 9 "nine"
    , Item 3 "3"
    , Item 0 "zero"
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model dnd.model initList, Cmd.none )


type Msg
    = Dropped Int ( Int, String )
    | DnDMsg DraggableMsg


move items from to =
    if to < from then
        moveLeft items from (to - 1)

    else
        moveRight items from (to - 1)



--   to    from
--   |     |
-- 1 2 3 4 5 6 7
--
--[1,2] ++ [5] ++ [3,4] ++ [6,7]


moveLeft : List Item -> Int -> Int -> List Item
moveLeft items from to =
    let
        first =
            List.take (to + 1) items

        item =
            items
                |> List.drop from
                |> List.take 1

        middle =
            items
                |> List.drop (to + 1)
                |> List.take (from - to - 1)

        rest =
            List.drop (from + 1) items
    in
    first ++ item ++ middle ++ rest



--  from   to
--   |     |
-- 1 2 3 4 5 6 7
--
--[1] ++ [3,4,5] ++ [2] ++ [6,7]


moveRight : List Item -> Int -> Int -> List Item
moveRight items from to =
    let
        first =
            List.take from items

        item =
            items
                |> List.drop from
                |> List.take 1

        middle =
            items
                |> List.drop (from + 1)
                |> List.take (to - from)

        rest =
            List.drop (to + 1) items
    in
    first ++ middle ++ item ++ rest


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    ( update_ msg model, Cmd.none )


update_ : Msg -> Model -> Model
update_ msg model =
    case msg of
        Dropped to ( from, _ ) ->
            { model | items = move model.items from to }

        DnDMsg msg_ ->
            { model | draggable = DnD.update msg_ model.draggable }


view : Model -> Html Msg
view model =
    let
        itemValues =
            model.items |> List.map .value
    in
    div
        [ (\( a, b ) -> style a b) ( "width", "100%" )
        , (\( a, b ) -> style a b) ( "height", "25px" )
        ]
        ((model.items
            |> List.indexedMap
                (\index item ->
                    [ droppable index model.draggable
                    , dnd.draggable ( index, item.repr ) [] [ box "solid" item.repr ]
                    ]
                )
            |> List.concat
         )
            ++ [ droppable (List.length model.items)
                    model.draggable
               , br [] []
               , p []
                    [ text <|
                        if itemValues == (itemValues |> List.sort) then
                            "The list is sorted"

                        else
                            "The list is messed"
                    ]
               , DnD.dragged
                    model.draggable
                    (box "dotted" << Tuple.second)
               ]
        )


droppable index draggableModel =
    dnd.droppable
        index
        [ (\( a, b ) -> style a b) ( "width", "40px" )
        , (\( a, b ) -> style a b) ( "height", "20px" )
        , (\( a, b ) -> style a b) ( "float", "left" )
        , (\( a, b ) -> style a b)
            ( "background-color"
            , if
                case DnD.getDropMeta draggableModel of
                    Just to ->
                        to == index

                    _ ->
                        False
              then
                "cyan"

              else
                "white"
            )
        ]
        [ box "solid" "" ]


box : String -> String -> Html Msg
box border value =
    div
        [ (\( a, b ) -> style a b) ( "height", "20px" )
        , (\( a, b ) -> style a b) ( "width", "40px" )
        , (\( a, b ) -> style a b) ( "float", "left" )
        , (\( a, b ) -> style a b) ( "border", "1px " ++ border ++ " black" )
        ]
        [ text value ]
