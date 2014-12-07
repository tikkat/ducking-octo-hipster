import Control.Monad (when)

import Haste hiding (eval)
import Haste.Graphics.Canvas

import Data.Maybe

import Expr
import Pages

canWidth  = 300
canHeight = 300
sca = 0.04

readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw elem can = do
  input <- getProp elem "value"
  let exp = fromMaybe X (readExpr input)
  let allPoints = points exp sca (canWidth, canHeight)
  render can (stroke (path allPoints))

main = do
  -- Elements
  canvas  <- mkCanvas canWidth canHeight   -- The drawing area
  fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
  input   <- mkInput 20 "x"                -- The formula input
  draw    <- mkButton "Draw graph"         -- The draw button
    -- The markup "<i>...</i>" means that the text inside should be rendered
    -- in italics.

  -- Layout
  formula <- mkDiv
  row formula [fx, input]
  column documentBody [canvas, formula, draw]

  -- Styling
  setStyle documentBody "backgroundColor" "lightblue"
  setStyle documentBody "textAlign" "center"
  setStyle input "fontSize" "14pt"
  focus input
  select input

  -- Interaction
  Just can <- getCanvas canvas
  onEvent draw  OnClick $ \_ _  -> readAndDraw input can
  onEvent input OnKeyUp $ \code -> when (code == 13) $ readAndDraw input can
    -- "Enter" key has code 13