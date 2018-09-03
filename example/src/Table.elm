module Table exposing (Id, Item, Model, Msg(..), addToLeft, addToRight, box, dndLeft, dndRigth, dragged, init, main, subscriptions, update, update_, view)

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


dndLeft =
    DnD.init DnDMsgLeftColumn (always DropToLeft)


dndRigth =
    DnD.init DnDMsgRightColumn (always DropToRight)


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
    , draggableLeft : DnD.Draggable () Item
    , draggableRight : DnD.Draggable () Item
    }


init : () -> ( Model, Cmd Msg )
init _ =
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
    | DnDMsgLeftColumn (DnD.Msg () Item)
    | DnDMsgRightColumn (DnD.Msg () Item)


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

        DnDMsgLeftColumn msg_ ->
            { model | draggableLeft = DnD.update msg_ model.draggableLeft }

        DnDMsgRightColumn msg_ ->
            { model | draggableRight = DnD.update msg_ model.draggableRight }


view : Model -> Html Msg
view model =
    div [ (\( a, b ) -> style a b) ( "width", "100%" ) ]
        [ dndLeft.droppable ()
            [ (\( a, b ) -> style a b) ( "width", "50%" )
            , (\( a, b ) -> style a b) ( "min-height", "200px" )
            , (\( a, b ) -> style a b) ( "float", "left" )
            , (\( a, b ) -> style a b)
                ( "background-color"
                , case DnD.getDropMeta model.draggableLeft of
                    Just _ ->
                        "cyan"

                    _ ->
                        "white"
                )
            ]
            (List.map
                (\item -> dndRigth.draggable item [] [ box item ])
                model.left
            )
        , dndRigth.droppable ()
            [ (\( a, b ) -> style a b) ( "width", "50%" )
            , (\( a, b ) -> style a b) ( "min-height", "200px" )
            , (\( a, b ) -> style a b) ( "float", "right" )
            , (\( a, b ) -> style a b)
                ( "background-color"
                , case DnD.getDropMeta model.draggableRight of
                    Just _ ->
                        "cyan"

                    _ ->
                        "white"
                )
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
