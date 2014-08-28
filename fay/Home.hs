module Home where

import           FFI
import           Hexagon
import           Language.Fay.Yesod
import           Prelude
import           RaphaelJS
import           SharedTypes

data Event


addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"


testGrid :: Hexgrid
testGrid = Hexgrid {
  grid_orientation = Horizontal,
  grid_hex_size = 80,
  grid_width = 100,
  grid_height = 100
  }

onLoad :: Event -> Fay ()
onLoad _ = do
  putStrLn "Initializing ..."

  let g = testGrid
      initZoom = 8
      initX = 0
      initY = 0
  putStrLn "Creating Rendering Context ..."
  paper <-  raphael "map" 800 600
  setPaperViewBox paper g (initX, initY) initZoom
  putStrLn "Creating Map Objects ..."
  es <- renderGrid paper g
  putStrLn "The document has loaded"
  return ()

main :: Fay ()
main = do
  putStrLn "Hello Console!"
  addWindowEvent "load" onLoad
  return ()
