module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Mouse
import DnD
import Debug


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


type RightColumn
    = RightColumn


type LeftColumn
    = LeftColumn


type alias Id =
    Int


type alias Item =
    { id : Id
    , text : String
    }


type alias Model =
    { left : List Item
    , right : List Item
    , draggableLeft : DnD.Dragabble LeftColumn Item
    , draggableRight : DnD.Dragabble RightColumn Item
    }


init : ( Model, Cmd.Cmd Msg )
init =
    ( Model
        [ { id = 1, text = "hello" }, { id = 2, text = "world" } ]
        [ { id = 3, text = "elm" }, { id = 4, text = "cool" } ]
        Nothing
        Nothing
    , Cmd.none
    )


type Msg
    = DropToRight RightColumn Item Mouse.Position
    | DropToLeft LeftColumn Item Mouse.Position
    | DnDMsgLeftColumn (DnD.Msg LeftColumn Item)
    | DnDMsgRightColumn (DnD.Msg RightColumn Item)


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    ( update' msg model, Cmd.none )


addToLeft model item =
    { model | left = model.left ++ [ item ] }


addToRight model item =
    { model | right = model.right ++ [ item ] }


update' : Msg -> Model -> Model
update' msg model =
    case Debug.log "msg" msg of
        DropToLeft _ item _ ->
            addToLeft model item

        DropToRight _ item _ ->
            addToRight model item

        DnDMsgLeftColumn msg ->
            { model | draggableLeft = DnD.update msg model.draggableLeft }

        DnDMsgRightColumn msg ->
            { model | draggableRight = DnD.update msg model.draggableRight }


view : Model -> Html Msg
view model =
    div []
        [ DnD.dropable DnDMsgLeftColumn
            LeftColumn
            (div
                []
                (List.map
                    (DnD.dragable DnDMsgRightColumn RightColumn box)
                    model.left
                )
            )
        , DnD.dropable DnDMsgRightColumn
            RightColumn
            (div
                []
                (List.map
                    (DnD.dragable DnDMsgLeftColumn LeftColumn box)
                    model.right
                )
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
