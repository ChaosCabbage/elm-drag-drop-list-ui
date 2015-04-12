module DragDropList (main) where

import Color
import Graphics.Element (..)
import Graphics.Input
import List
import Text
import DragAndDrop
import Signal
import Debug

hover = Signal.channel Nothing

hoverWatch = Debug.watch "hover" (Signal.subscribe hover)

type DragItem = Dragging Int (Int,Int) | NotDragging

type alias Model =
  { list : List Element
  , gap : Element
  }

dragPadElement : Element
dragPadElement = spacer 30 40 |> color Color.black

dragPad id =
  let action hovering = Debug.watch "hover" <| case hovering of
    True -> Just id
    False -> Nothing
  in
    Graphics.Input.hoverable (Signal.send hover << action) dragPadElement

listWithDragpads list =
  let (dragW,dragH) = sizeOf dragPadElement
      positionedPad id height =
        container dragW height middle <| dragPad id
      idList = List.indexedMap (,) list
      makeIdPad = (\(i,x) -> [positionedPad i <| heightOf x, x])
  in
      List.map makeIdPad idList

sameSizedSpacer element =
  let (x,y) = sizeOf element
  in spacer x y

listWithFloater list gapId (xoff,yoff)=
  let beforeGap = List.take gapId list
      (gapElement::afterGap) = List.drop gapId list
      (x,y) = sizeOf gapElement
      pos = topLeftAt (absolute xoff) (absolute yoff)
      floater = container (x+xoff) (y+yoff) pos gapElement
  in
    beforeGap ++ [floater] ++ afterGap

gapElement model =
  let (dragW,dragH) = sizeOf dragPadElement
      (gapW,gapH) = sizeOf model.gap
      maxWidth = List.foldr (\el -> widthOf el |> max) 0 model.list
  in
    container maxWidth gapH middle model.gap
    |> container (maxWidth + dragW) gapH midRight

listElements model =
  let (dragW,dragH) = sizeOf dragPadElement
  in
    List.map (flow right) <| listWithDragpads model.list

view : Model -> DragItem -> Element
view model dragging =
  let pureElements = listElements model
      elements = case dragging of
          Dragging id (x,y) -> listWithFloater pureElements id (x,y)
          NotDragging -> pureElements
  in flow down <| List.intersperse (gapElement model) elements



sampleElement =
  container 200 100 middle (Text.plainText "What")
    |> color Color.yellow

sampleList =
  List.repeat 7 sampleElement

sampleGap =
  spacer 100 10
    |> color Color.red

moveBy (dx,dy) (x,y) = (x + dx, y - dy)

dragDropUpdate m =
  let moo = Debug.watch "dragDrop" m
  in case moo of
    Just (i, DragAndDrop.MoveBy (dx,dy)) -> moveBy (dx,-dy)
    Just (i, DragAndDrop.Release)        -> \(x,y) -> (0,0)
    _                                    -> identity

dragStatus dragDrop movement =
  case dragDrop of
    Nothing                       -> NotDragging
    Just (_, DragAndDrop.Release)  -> NotDragging
    Just (i, _)                   -> Dragging i movement
  |> Debug.watch "drag"


main =
  let track = DragAndDrop.trackMany Nothing (Signal.subscribe hover)
      totalDrag = Signal.foldp dragDropUpdate (0,0) track
      drag = Signal.map2 dragStatus track totalDrag
      see = view { list = sampleList, gap = sampleGap }
  in
    Signal.map see drag
