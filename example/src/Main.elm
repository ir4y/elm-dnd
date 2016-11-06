module Main exposing (..)

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
        [ DnD.subscriptions DropToLeft DnDMsgLeftColumn model.draggableLeft
        , DnD.subscriptions DropToRight DnDMsgRightColumn model.draggableRight
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
    , draggableLeft : DnD.Draggable Item
    , draggableRight : DnD.Draggable Item
    }


init : ( Model, Cmd.Cmd Msg )
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
    | DnDMsgLeftColumn (DnD.Msg Item)
    | DnDMsgRightColumn (DnD.Msg Item)


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
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


wrapDraggable : (DnD.Msg Item -> Msg) -> (Item -> Html Msg) -> Item -> Html Msg
wrapDraggable cmdWrap view item =
    DnD.draggable cmdWrap item [] [ view item ]


(=>) =
    (,)


view : Model -> Html Msg
view model =
    div [ style [ "width" => "100%" ] ]
        [ DnD.droppable DnDMsgLeftColumn
            [ style
                ([ "width" => "50%"
                 , "min-height" => "200px"
                 , "float" => "left"
                 ]
                    ++ if DnD.atDroppable model.draggableLeft then
                        [ "background-color" => "cyan" ]
                       else
                        []
                )
            ]
            (List.map
                (wrapDraggable DnDMsgRightColumn box)
                model.left
            )
        , DnD.droppable DnDMsgRightColumn
            [ style
                ([ "width" => "50%"
                 , "min-height" => "200px"
                 , "float" => "right"
                 ]
                    ++ if DnD.atDroppable model.draggableRight then
                        [ "background-color" => "cyan" ]
                       else
                        []
                )
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
