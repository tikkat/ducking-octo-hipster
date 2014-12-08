import Control.Monad (when)

import Haste hiding (eval)
import Haste.Prim
import Haste.Foreign
import Haste.Graphics.Canvas hiding (scale)

import Data.Maybe

import Expr

canWidth  = 300
canHeight = 300

scale = 0.04

readAndDraw :: Elem -> Elem -> Canvas -> IO ()
readAndDraw expInput scaleInput can = do
  exp           <- getProp expInput "value"
  inputScale    <- getProp scaleInput "value"
  let exp'      = readExpr exp
  let allPoints = if isJust exp' then points (fromJust exp') (scale / read inputScale) (canWidth, canHeight) else []
  render can $ stroke $ path allPoints

readAndDifferentiate :: Elem -> Elem -> Canvas -> IO ()
readAndDifferentiate expInput scaleInput can = do
  exp <- getProp expInput "value"
  let exp' = readExpr exp

  when (isJust exp') $ do
    setProp expInput "value" (showExpr (differentiate (fromJust exp')))
    readAndDraw expInput scaleInput can

main = do
  setStyle documentBody "padding" "40px"
  setStyle documentBody "background-color" "#efefef"
  setStyle documentBody "text-align" "center"

  header <- newHeader "Function plotter"

  canvas <- newCanvas canWidth canHeight

  (expElem, expInput) <- newInput "x" "f(x) ="
  (scaleElem, scaleInput) <- newInput "1" "Scale:"

  drawButton <- newButton "Draw"
  diffButton <- newButton "Differentiate"

  buttonDiv <- newElem "div"
  setChildren buttonDiv [diffButton, drawButton]

  setChildren documentBody [header, canvas, expElem, scaleElem, buttonDiv]

  focus expInput
  select expInput

  Just can <- getCanvas canvas
  onEvent drawButton  OnClick $ \_ _  -> readAndDraw expInput scaleInput can
  onEvent expInput    OnKeyUp $ \code -> when (code == 13) $ readAndDraw expInput scaleInput can
  onEvent diffButton  OnClick $ \_ _  -> readAndDifferentiate expInput scaleInput can

newCanvas :: Int -> Int -> IO Elem
newCanvas width height = do
  canvas <- newElem "canvas"
  setProp canvas "width" (show width)
  setProp canvas "height" (show height)
  setStyle canvas "margin" "auto"
  setStyle canvas "display" "block"
  setStyle canvas "background-color" "white"
  return canvas

newInput :: String -> String -> IO (Elem, Elem)
newInput value label = do
  inputElem <- newElem "input"
  setProp inputElem "type" "text"
  setProp inputElem "value" value
  setStyle inputElem "width" "240px"
  setStyle inputElem "height" "25px"
  setStyle inputElem "line-height" "25px"
  setStyle inputElem "border" "0"
  setStyle inputElem "padding" "0"
  setStyle inputElem "outline" "none"
  setStyle inputElem "font-family" "arial"
  setStyle inputElem "font-size" "14px"

  labelElem <- newElem "span"
  setProp labelElem "innerHTML" label
  setStyle labelElem "display" "inline-block"
  setStyle labelElem "width" "40px"
  setStyle labelElem "text-align" "left"
  setStyle labelElem "font-family" "arial"
  setStyle labelElem "font-size" "14px"
  setStyle labelElem "color" "#bbbbbb"

  div <- newElem "div"
  setStyle div "display" "inline-block"
  setStyle div "width" "280px"
  setStyle div "margin-top" "10px"
  setStyle div "padding" "0px 10px"
  setStyle div "background-color" "white"

  div' <- newElem "div"

  setChildren div [labelElem, inputElem]
  setChildren div' [div]

  return (div', inputElem)

newButton :: String -> IO Elem
newButton value = do
  button <- newElem "div"
  setProp button "innerText" value
  setStyle button "display" "inline-block"
  setStyle button "margin" "20px 5px"
  setStyle button "height" "25px"
  setStyle button "line-height" "25px"
  setStyle button "padding" "0px 10px"
  setStyle button "cursor" "pointer"
  setStyle button "background-color" "black"
  setStyle button "font-family" "arial"
  setStyle button "color" "white"
  return button

newHeader :: String -> IO Elem
newHeader text = do
  header <- newElem "div"
  setProp header "innerText" text
  setStyle header "margin-bottom" "30px"
  setStyle header "font-family" "arial"
  setStyle header "font-size" "50px"
  setStyle header "color" "#666"
  return header

select :: Elem -> IO ()
select = ffi $ toJSStr "(function(e) {e.select();})"