import Control.Monad (when)

import Haste hiding (eval)
import Haste.Graphics.Canvas

import Data.Maybe

import Expr
import Pages

canWidth  = 300
canHeight = 300

readAndDraw :: Elem -> Elem -> Canvas -> IO ()
readAndDraw expInput scaInput can = do
  exp <- getProp expInput "value"
  sca <- getProp scaInput "value"
  let exp' = readExpr exp
  let allPoints = if isJust exp' then points (fromJust exp') (0.04 / read sca) (canWidth, canHeight) else []
  render can $ stroke $ path allPoints

readAndDifferentiate :: Elem -> Elem -> Canvas -> IO ()
readAndDifferentiate expInput scaInput can = do
  exp <- getProp expInput "value"
  let exp' = readExpr exp

  when (isJust exp') $ do
    setProp expInput "value" (showExpr (differentiate (fromJust exp')))
    readAndDraw expInput scaInput can

main = do
  -- Elements
  canvas    <- mkCanvas canWidth canHeight      -- The drawing area
  fx        <- mkHTML "<i>f</i>(<i>x</i>) = "   -- The text "f(x) = "
  expElem   <- mkInput 20 "x"                   -- The formula input
  scaTxt    <- mkHTML "Scale: "                 -- The text "Scale: "
  scaElem   <- mkInput 10 "1"                   -- The scale input
  drawBtn   <- mkButton "Draw graph"            -- The draw button
  diffBtn   <- mkButton "Differentiate"         -- The differentiate button
  br        <- mkHTML "<br />"                  -- A line break

  -- Layout
  formula <- mkDiv
  formula' <- mkDiv
  formula'' <- mkDiv
  row formula [fx, expElem]
  row formula' [scaTxt, scaElem]
  column documentBody [canvas, formula, formula', br, diffBtn, drawBtn]

  -- Styling
  setStyle documentBody "backgroundColor" "lightblue"
  setStyle documentBody "textAlign" "center"
  setStyle expElem "fontSize" "14pt"
  setStyle scaElem "fontSize" "14pt"
  setStyle diffBtn "fontSize" "11pt"
  setStyle drawBtn "fontSize" "11pt"

  focus expElem
  select expElem

  -- Interaction
  Just can <- getCanvas canvas
  onEvent drawBtn   OnClick $ \_ _  -> readAndDraw expElem scaElem can
  onEvent expElem   OnKeyUp $ \code -> when (code == 13) $ readAndDraw expElem scaElem can
  onEvent diffBtn   OnClick $ \_ _  -> readAndDifferentiate expElem scaElem can