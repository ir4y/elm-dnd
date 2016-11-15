module DragOnly exposing (..)

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
        [ DnD.subscriptions DnDMsg model
        ]


type alias Model =
    DnD.Draggable String Msg


init : ( Model, Cmd Msg )
init =
    ( DnD.init, Cmd.none )


type Msg
    = DnDMsg (DnD.Msg String Msg)


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update (DnDMsg msg) model =
    ( DnD.update msg model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ DnD.draggable "drag-n-drop" DnDMsg [] [ text "hello" ]
        , DnD.dragged
            model
            dragged
        ]


dragged : String -> Html Msg
dragged item =
    div [] [ text item ]
