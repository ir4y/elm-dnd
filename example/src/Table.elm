module Table exposing (..)

import Html.App as Html
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
        [ DnD.subscriptions DnDMsgLeftColumn model.draggableLeft
        , DnD.subscriptions DnDMsgRightColumn model.draggableRight
        ]


type alias Id =
    Int


type alias Item =
    { id : Id
    , text : String
    }


type alias Model =
    { left : List Item
    , right : List Item
    , draggableLeft : DnD.Draggable Item Msg
    , draggableRight : DnD.Draggable Item Msg
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        [ { id = 1, text = "hello" }, { id = 2, text = "world" } ]
        [ { id = 3, text = "elm" }, { id = 4, text = "is" }, { id = 5, text = "cool" } ]
        Nothing
        Nothing
    , Cmd.none
    )


type Msg
    = DropToRight Item
    | DropToLeft Item
    | DnDMsgLeftColumn (DnD.Msg Item Msg)
    | DnDMsgRightColumn (DnD.Msg Item Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( update' msg model, Cmd.none )


addToLeft model item =
    { model
        | left = model.left ++ [ item ]
        , right = List.filter (\i -> i.id /= item.id) model.right
    }


addToRight model item =
    { model
        | right = model.right ++ [ item ]
        , left = List.filter (\i -> i.id /= item.id) model.left
    }


update' : Msg -> Model -> Model
update' msg model =
    case msg of
        DropToLeft item ->
            addToLeft model item

        DropToRight item ->
            addToRight model item

        DnDMsgLeftColumn msg ->
            { model | draggableLeft = DnD.update msg model.draggableLeft }

        DnDMsgRightColumn msg ->
            { model | draggableRight = DnD.update msg model.draggableRight }


wrapDraggable : (DnD.Msg Item Msg -> Msg) -> (Item -> Html Msg) -> Item -> Html Msg
wrapDraggable cmdWrap view item =
    DnD.draggable cmdWrap item [] [ view item ]


(=>) =
    (,)


view : Model -> Html Msg
view model =
    div [ style [ "width" => "100%" ] ]
        [ DnD.droppable DnDMsgLeftColumn
            DropToLeft
            [ style
                [ "width" => "50%"
                , "min-height" => "200px"
                , "float" => "left"
                , "background-color"
                    => case DnD.atDroppable model.draggableLeft of
                        Just (DropToLeft _) ->
                            "cyan"

                        _ ->
                            "white"
                ]
            ]
            (List.map
                (wrapDraggable DnDMsgRightColumn box)
                model.left
            )
        , DnD.droppable DnDMsgRightColumn
            DropToRight
            [ style
                [ "width" => "50%"
                , "min-height" => "200px"
                , "float" => "right"
                , "background-color"
                    => case DnD.atDroppable model.draggableRight of
                        Just (DropToRight _) ->
                            "cyan"

                        _ ->
                            "white"
                ]
            ]
            (List.map
                (wrapDraggable DnDMsgLeftColumn box)
                model.right
            )
        , DnD.dragged
            model.draggableLeft
            dragged
        , DnD.dragged
            model.draggableRight
            dragged
        ]


box : Item -> Html Msg
box item =
    p [] [ text item.text ]


dragged : Item -> Html Msg
dragged item =
    p [] [ text item.text ]
