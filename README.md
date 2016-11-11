# elm-dnd

This library allows you to build great UI with drag-and-drop simple.  
It is abstracting you from mouse events and other low-level staff.  
You can operate high-level things such as draggable and droppable areas.  

The idea of package API is you should be able to wrap elements with `draggable met`a to add an ability to drag it.  
The dragget object will get some meta information.  
Also, you could wrap another element with `droppable OnDrop`,  
so if you drop element over that element, the message `OnDrop meta` will be fired.  

Actually, we need a message wrapper so the actual signature will be  
`droppable OnDrop DnDMsg` and `OnDrop meta DnDMsg`.  
this functions will generate a view function which will render `div` tag with handlers for drag and drop mouse events.  

So finally API will be  
```elm
droppable : (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
droppable = DnD.droppable Dropped DnDMsg

draggable : (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
draggable =  DnD.draggable meta DnDMsg


view = div []
    [ draggable [class "drag-me"] ["drag me please"]
    , droppable [class "drop-here"] []
    ]
```

You can find examples [here](https://github.com/ir4y/elm-dnd/tree/master/example/src).  


