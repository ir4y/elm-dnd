module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import DnD


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.none



--Sub.batch
--[ DnD.subscriptions model.draggableLeft
--, DnD.subscriptions model.draggableRight
--]


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
    ( Model [] [] Nothing Nothing, Cmd.none )


type Msg
    = DropToRight Item
    | DropToLeft Item
    | DnDMsgLeftColumn DnD.Msg
    | DnDMsgRightColumn DnD.Msg


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    ( update' msg model, Cmd.none )


addToLeft model item =
    { model | left = model.left ++ [ item ] }


addToRight model item =
    { model | left = model.right ++ [ item ] }


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


view : Model -> Html Msg
view model =
    div []
        [ DnD.dropable LeftColumn
            (div
                []
                (List.map
                    (DnD.dragable RightColumn << box)
                    model.left
                )
            )
        , DnD.dropable RightColumn
            (div
                []
                (List.map
                    (DnD.dragable LeftColumn << box)
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
