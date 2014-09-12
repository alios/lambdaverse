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



renderGrid :: Hexgrid -> ScreenBox -> Offset -> Paper -> Fay [Element]
renderGrid g vb offset paper =
  let str c = ((show . offset_q $ c) ++ "/" ++ (show . offset_r $ c))
      cs = hexesFromScreenBox g offset vb
      render c = renderHexagon paper g (str c) (offset2screen g c)
  in sequence [ render c | c <- cs ]

testGrid :: Hexgrid
testGrid = Hexgrid {
  grid_orientation = Horizontal,
  grid_hex_size = 64,
  grid_width = 100,
  grid_height = 100
  }


onLoad :: Event -> Fay ()
onLoad _ = do
  putStrLn "Initializing ..."

  let g = testGrid
      px = 700
      py = 400
      z = 2
      vb = mkScreenBox (-100) (-100) (px * z) (py * z)
  putStrLn "Creating Rendering Context ..."
  paper <-  raphael "map" px py
  setPaperViewBox paper vb
  putStrLn "Creating Map Objects ..."
  es <- renderGrid g vb Even paper
  putStrLn $ "The document has loaded " ++ show (length es) ++ " hexes."
  return ()

main :: Fay ()
main = do
  putStrLn "Hello Console!"
  addWindowEvent "load" onLoad
  return ()
