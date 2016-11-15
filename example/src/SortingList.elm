module SortingList exposing (..)

import Html
import Html.Attributes exposing (style)
import Html exposing (..)
import DnD


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ DnD.subscriptions DnDMsg model.draggable
        ]


type alias Draggable =
    DnD.Draggable ( Int, String ) Msg


type alias DraggableMsg =
    DnD.Msg ( Int, String ) Msg


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


init : ( Model, Cmd Msg )
init =
    ( Model DnD.init initList, Cmd.none )


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

        DnDMsg msg ->
            { model | draggable = DnD.update msg model.draggable }


(=>) =
    (,)


view : Model -> Html Msg
view model =
    let
        itemValues =
            model.items |> List.map .value
    in
        div
            [ style
                [ "width" => "100%"
                , "height" => "25px"
                ]
            ]
            ((model.items
                |> List.indexedMap
                    (\index item ->
                        [ droppable index model.draggable
                        , DnD.draggable ( index, item.repr ) DnDMsg [] [ box "solid" item.repr ]
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
    DnD.droppable
        (Dropped index)
        DnDMsg
        [ style
            [ "width" => "40px"
            , "height" => "20px"
            , "float" => "left"
            , "background-color"
                => if
                    case DnD.atDroppable draggableModel of
                        Just (Dropped to _) ->
                            to == index

                        _ ->
                            False
                   then
                    "cyan"
                   else
                    "white"
            ]
        ]
        [ box "solid" "" ]


box : String -> String -> Html Msg
box border value =
    div
        [ style
            [ "height" => "20px"
            , "width" => "40px"
            , "float" => "left"
            , "border" => ("1px " ++ border ++ " black")
            ]
        ]
        [ text value ]
