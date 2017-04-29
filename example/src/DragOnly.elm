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


dnd =
    DnD.init DnDMsg never


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dnd.subscriptions model
        ]


type alias Model =
    DnD.Draggable Never String


init : ( Model, Cmd Msg )
init =
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
