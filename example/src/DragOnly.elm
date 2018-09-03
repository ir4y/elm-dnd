module DragOnly exposing (Model, Msg(..), dnd, dragged, init, main, subscriptions, update, view)

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
    DnD.init DnDMsg never


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dnd.subscriptions model
        ]


type alias Model =
    DnD.Draggable Never String


init : () -> ( Model, Cmd Msg )
init _ =
    ( dnd.model, Cmd.none )


type Msg
    = DnDMsg (DnD.Msg Never String)


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update (DnDMsg msg) model =
    ( DnD.update msg model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ dnd.draggable "drag-n-drop" [] [ text "hello" ]
        , DnD.dragged
            model
            dragged
        ]


dragged : String -> Html Msg
dragged item =
    div [] [ text item ]
