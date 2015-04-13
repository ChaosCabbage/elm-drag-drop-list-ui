module DragDropList (main) where

import Color
import Graphics.Element (..)
import Graphics.Collage (..)
import Graphics.Input
import List
import Text
import DragAndDrop
import Signal
import Debug
import Array

-- Signals

hover = Signal.channel Nothing

-- Models

type DragItem = Dragging Int (Int,Int) | NotDragging

type alias Model =
  { list : List Element
  , gap : Element
  }

-- View

dragPadElement : Element
dragPadElement = spacer 30 40 |> color Color.black

dragPad id =
  let action hovering = Debug.watch "hover" <|
    case hovering of
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

gapElement model =
  let (dragW,dragH) = sizeOf dragPadElement
      (gapW,gapH) = sizeOf model.gap
      maxWidth = List.map widthOf model.list |> List.maximum
  in
    container maxWidth gapH middle model.gap
    |> container (maxWidth + dragW) gapH midRight

listElements model =
  let (dragW,dragH) = sizeOf dragPadElement
  in
    List.map (flow right) <| listWithDragpads model.list

itemYPositions model =
  let gapHeight = heightOf model.gap
      heightOfElement e = (heightOf e) + gapHeight
      heights = List.map heightOfElement model.list
  in List.take (List.length heights) (List.scanl (+) 0 heights)

itemYPosition : Model -> Int -> Int
itemYPosition model id =
    List.head (List.drop id (itemYPositions model))

insertionPoints model =
  let points = itemYPositions model
      halfHeights = List.map (\el -> (heightOf el // 2)) model.list
  in List.map2 (+) points halfHeights

drawInsertionPoints model =
  let points = insertionPoints model |> Debug.watch "insertion points"
      drawLine = rect 100 5
                  |> filled Color.red
                  |> alpha 0.7
      drawLineAt pos = drawLine |> move (0, ((toFloat h)/2) - toFloat pos)
      h = if (List.isEmpty points) then
            0
          else
            ((List.head <| List.reverse points) + 5)
  in collage 100 h (
    (List.map drawLineAt points)
    ++
    [rect 100 (toFloat h) |> filled Color.orange |> alpha 0.4]
  )

currentInsertionPoint : Model -> Int -> Int -> Int
currentInsertionPoint model gapId yOffset =
  let points = insertionPoints model
      originalTop = itemYPosition model gapId
      pointIndexes = List.indexedMap (,) points
      afterHeight = List.filter (\(_,x) -> x > originalTop+yOffset) pointIndexes
  in
    if (List.isEmpty afterHeight) then
      (List.length pointIndexes)-1
    else
      fst (List.head afterHeight)


listWithSpace list gapId insertionPoint =
  let beforeSpace = List.take gapId list
      (spaceElement::afterSpace) = List.drop gapId list
      newList = beforeSpace ++ afterSpace
      beforeInsertion = List.take insertionPoint newList
      afterInsertion = List.drop insertionPoint newList
  in
    beforeInsertion ++ [sameSizedSpacer spaceElement] ++ afterInsertion

drawListWithFloatingGap model dragging =
  let pureElements = listElements model
      elements = case dragging of
          Dragging id (x,y) ->
            listWithSpace pureElements id
                (Debug.watch "current insert point"
                (currentInsertionPoint model id y))
          NotDragging -> pureElements
      withGaps = List.intersperse (gapElement model) elements
  in
    flow down withGaps

drawFloatingItem model dragging =
  case dragging of
    NotDragging -> empty
    Dragging id (x,y) ->
      let (xMax,yMax) = sizeOf <| drawListWithFloatingGap model dragging
          element = flow right [List.head (List.drop id model.list)]
          yPos = -((itemYPosition model id) + y + ((heightOf element) // 2) - (yMax//2))
          floatingForm = toForm element |> move (toFloat x, toFloat yPos)
      in collage xMax yMax [floatingForm]



view : Model -> DragItem -> Element
view model dragging =
  layers [
    drawListWithFloatingGap model dragging,
--    drawInsertionPoints model,
    drawFloatingItem model dragging
  ]



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
    Just (_, DragAndDrop.Lift)    -> NotDragging
    Just (_, DragAndDrop.Release) -> NotDragging
    Just (i, _)                   -> Dragging i movement
  |> Debug.watch "drag"


sampleElement text =
  container 200 100 middle (Text.plainText text)
    |> color Color.yellow

sampleList =
  List.map sampleElement ["Who", "What", "When", "Where", "How"]

sampleGap =
  spacer 100 10
    |> color Color.red

main =
  let track = DragAndDrop.trackMany Nothing (Signal.subscribe hover)
      totalDrag = Signal.foldp dragDropUpdate (0,0) track
      drag = Signal.map2 dragStatus track totalDrag
      see = view { list = sampleList, gap = sampleGap }
  in
    Signal.map see drag
