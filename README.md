# elm-dnd

This library allows you to build great UI with drag-and-drop simple.  
It is abstracting you from mouse events and other low-level staff.  
You can operate high-level things such as draggable and droppable areas.  

The idea of package API is you should be able to wrap elements with `draggable met`a to add an ability to drag it.  
The dragget object will get some meta information.  
Also, you could wrap another element with `droppable OnDrop`,  
so if you drop element over that element, the message `OnDrop meta` will be fired.  

At first you need to initialize draggable state and function.  
`DnD.init` helper returns initModel, subscription, draggable and droppable functions for your message wrapper.  

```
type Msg
    = NoOp
    ..
    | Dropped String
    | DnDMsg (DnD.Msg String Msg)


dnd = DnD.init DnDWrapper
type alias Model =
    { ...
    , draggable = dnd.model
    }
```

Subscriptions alow you to get drop event.
```
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dnd.subscriptions model.draggable
        ]
```
View wrapper for draggable object, you could drag object wraped by this helper
```
draggable
    : (Html.Attribute Msg)
    -> List (Html Msg)
    -> Html Msg
draggable =  dnd.draggable meta
```
View helper for droppable area, you could drop object to this area,
after that your on `Drop meta` message will be invoked.
```
droppable
  : (Html.Attribute Msg)
  -> List (Html Msg)
  -> Html Msg
droppable = dnd.droppable Dropped
```

You can find examples [here](https://github.com/ir4y/elm-dnd/tree/master/example/src).  


