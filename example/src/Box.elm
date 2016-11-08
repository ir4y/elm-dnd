module Box exposing (..)

import Html.App as Html
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions, onMouseEnter, onMouseLeave)
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
        [ DnD.subscriptions DnDMsg model.draggable
        ]


type alias Model =
    { draggable : DnD.Draggable Int Msg
    , count : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing 0, Cmd.none )


type Msg
    = Dropped Int
    | DnDMsg (DnD.Msg Int Msg)
    | EnterDroppable


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    ( update' msg model, Cmd.none )


update' : Msg -> Model -> Model
update' msg model =
    case msg of
        EnterDroppable ->
            { model | count = model.count + 1 }

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
            [ DnD.draggable DnDMsg (model.count + 1) [] [ dragged model.count ] ]
        , DnD.droppable DnDMsg
            Dropped
            [ style
                [ "width" => "49%"
                , "min-height" => "200px"
                , "float" => "right"
                , "border" => "1px solid black"
                , "background-color"
                    => case DnD.atDroppable model.draggable of
                        Just (Dropped _) ->
                            "cyan"

                        _ ->
                            "white"
                ]
            , onMouseEnter EnterDroppable
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
