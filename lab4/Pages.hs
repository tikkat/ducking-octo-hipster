-- This module defines helper functions for creating web pages using HASTE

module Pages where

import Haste
import Haste.Prim
import Haste.Foreign

-- `mkDiv` makes a container element for grouping elements together
mkDiv :: IO Elem
mkDiv = newElem "div"

-- `wrapDiv e` makes a "div" node with `e` as the only child
wrapDiv :: Elem -> IO Elem
wrapDiv e = do
    d <- mkDiv
    addChild e d
    return d

-- `addChildren parent children` adds a list of children to a parent element
addChildren :: Elem -> [Elem] -> IO ()
addChildren parent children = sequence_ [addChild c parent | c <- children]

-- `row parent children` adds the children as a row row to the parent
-- (assuming the children do not use column layout internally)
row :: Elem -> [Elem] -> IO ()
row = addChildren

-- `column parent children` adds the children as a column column to the parent
column :: Elem -> [Elem] -> IO ()
column parent children = do
    cs <- sequence [wrapDiv c | c <- children]
    addChildren parent cs

-- `mkInput width init` makes an input element with the specified width and
-- initial text
mkInput :: Int -> String -> IO Elem
mkInput width init = do
    input <- newElem "input"
    setProp input "type" "text"
    setProp input "size" (show width)
    setProp input "value" init
    return input

-- `mkButton label` makes a clickable button with the given label
mkButton :: String -> IO Elem
mkButton label = do
    button <- newElem "button"
    setProp button "innerHTML" label
    return button

-- `mkHTML html` makes an element with the specified HTML content
mkHTML :: String -> IO Elem
mkHTML html = do
    elem <- newElem "span"
    setProp elem "innerHTML" html
    return elem

-- `select e` makes the text in element `e` selected
select :: Elem -> IO ()
select = ffi $ toJSStr "(function(e) {e.select();})"

-- `mkCanvas width height` makes a drawing canvas of the specified dimensions
mkCanvas :: Int -> Int -> IO Elem
mkCanvas width height = do
    canvas <- newElem "canvas"
    setStyle canvas "border" "1px solid black"
    setStyle canvas "backgroundColor" "white"
    setProp canvas "width" (show width)
    setProp canvas "height" (show height)
    return canvas