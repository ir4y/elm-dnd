# elm-dnd

This library allows you to build great UI with drag-and-drop simple.  
It is abstracting you from mouse events and other low-level stuff.  
You can operate high-level things such as draggable and droppable areas.  

The idea of package API is you should be able to wrap elements with `draggable dragMeta` to add an ability to drag it.  
The dragging object will get some meta information.  
Also, you could wrap another element with `droppable dropMeta`,  
so if you drop element over that element, the message `YourOnDropMessage dragMeta dropMeta` will be invoked.  

At first, you need to initialize draggable state and function.  
`DnD.init` helper returns initModel, subscription, draggable and droppable functions for your message wrapper and onDrop message.

```elm
type Msg
    = NoOp
    ..
    | OnDrop String Int
    | DnDMsg (DnD.Msg String Int)


type alias Model =
    { ...
    , draggable : DnD.Draggable String Int
    }


dnd = DnD.init DnDMsg OnDrop


model =
    { ...
    , draggable : dnd.model
    }
```

Subscriptions allow you to get drop event.
```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    dnd.subscriptions model.draggable
```
View wrapper for draggable object, you could drag object wrapped by this helper
```elm
draggable
    : (Html.Attribute Msg)
    -> List (Html Msg)
    -> Html Msg
draggable =  dnd.draggable dragMeta
```
View helper for droppable area, you could drop object to this area,
after that your on `OnDrop` message will be invoked.
```elm
droppable
  : (Html.Attribute Msg)
  -> List (Html Msg)
  -> Html Msg
droppable = dnd.droppable dropMeta
```

You can find simple examples [here](https://github.com/ir4y/elm-dnd/tree/master/example/src).  
For more complex example check [Chess Board](https://github.com/ir4y/elm-chess).  
