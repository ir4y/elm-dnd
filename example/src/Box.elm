module Box exposing (..)

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


init : ( Model, Cmd Msg )
init =
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

        DnDMsg msg ->
            { model | draggable = DnD.update msg model.draggable }


(=>) =
    (,)


view : Model -> Html Msg
view model =
    div [ style [ "width" => "100%" ] ]
        [ div
            [ style
                [ "width" => "49%"
                , "min-height" => "200px"
                , "float" => "left"
                , "border" => "1px solid black"
                ]
            ]
            [ dnd.draggable (model.count + 1) [] [ dragged model.count ] ]
        , dnd.droppable ()
            [ style
                [ "width" => "49%"
                , "min-height" => "200px"
                , "float" => "right"
                , "border" => "1px solid black"
                , "background-color"
                    => case DnD.getDropMeta model.draggable of
                        Just _ ->
                            "cyan"

                        _ ->
                            "white"
                ]
            ]
            []
        , DnD.dragged
            model.draggable
            dragged
        ]


dragged : Int -> Html Msg
dragged item =
    div
        [ style
            [ "height" => "20px"
            , "width" => "20px"
            , "border" => "1px dotted black"
            ]
        ]
        [ item |> toString |> text ]
