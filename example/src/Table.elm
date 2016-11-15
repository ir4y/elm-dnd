module Table exposing (..)

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


dndLeft =
    DnD.init DnDMsgLeftColumn


dndRigth =
    DnD.init DnDMsgRightColumn


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dndLeft.subscriptions model.draggableLeft
        , dndRigth.subscriptions model.draggableRight
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
        dndLeft.model
        dndRigth.model
    , Cmd.none
    )


type Msg
    = DropToRight Item
    | DropToLeft Item
    | DnDMsgLeftColumn (DnD.Msg Item Msg)
    | DnDMsgRightColumn (DnD.Msg Item Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( update_ msg model, Cmd.none )


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


update_ : Msg -> Model -> Model
update_ msg model =
    case msg of
        DropToLeft item ->
            addToLeft model item

        DropToRight item ->
            addToRight model item

        DnDMsgLeftColumn msg ->
            { model | draggableLeft = DnD.update msg model.draggableLeft }

        DnDMsgRightColumn msg ->
            { model | draggableRight = DnD.update msg model.draggableRight }


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


view : Model -> Html Msg
view model =
    div [ style [ "width" => "100%" ] ]
        [ dndLeft.droppable DropToLeft
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
                (\item -> dndRigth.draggable item [] [ box item ])
                model.left
            )
        , dndRigth.droppable DropToRight
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
                (\item -> dndLeft.draggable item [] [ box item ])
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
