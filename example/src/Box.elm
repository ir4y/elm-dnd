module Box exposing (Model, Msg(..), dnd, dragged, init, main, subscriptions, update, update_, view)

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
    DnD.init DnDMsg (always Dropped)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dnd.subscriptions model.draggable
        ]


type alias Model =
    { draggable : DnD.Draggable () Int
    , count : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model dnd.model 0, Cmd.none )


type Msg
    = Dropped Int
    | DnDMsg (DnD.Msg () Int)


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    ( update_ msg model, Cmd.none )


update_ : Msg -> Model -> Model
update_ msg model =
    case msg of
        Dropped item ->
            { model | count = item + 1 }

        DnDMsg msg_ ->
            { model | draggable = DnD.update msg_ model.draggable }


view : Model -> Html Msg
view model =
    div [ (\( a, b ) -> style a b) ( "width", "100%" ) ]
        [ div
            [ (\( a, b ) -> style a b) ( "width", "49%" )
            , (\( a, b ) -> style a b) ( "min-height", "200px" )
            , (\( a, b ) -> style a b) ( "float", "left" )
            , (\( a, b ) -> style a b) ( "border", "1px solid black" )
            ]
            [ dnd.draggable (model.count + 1) [] [ dragged model.count ] ]
        , dnd.droppable ()
            [ (\( a, b ) -> style a b) ( "width", "49%" )
            , (\( a, b ) -> style a b) ( "min-height", "200px" )
            , (\( a, b ) -> style a b) ( "float", "right" )
            , (\( a, b ) -> style a b) ( "border", "1px solid black" )
            , (\( a, b ) -> style a b)
                ( "background-color"
                , case DnD.getDropMeta model.draggable of
                    Just _ ->
                        "cyan"

                    _ ->
                        "white"
                )
            ]
            []
        , DnD.dragged
            model.draggable
            dragged
        ]


dragged : Int -> Html Msg
dragged item =
    div
        [ (\( a, b ) -> style a b) ( "height", "20px" )
        , (\( a, b ) -> style a b) ( "width", "20px" )
        , (\( a, b ) -> style a b) ( "border", "1px dotted black" )
        ]
        [ item |> Debug.toString |> text ]
