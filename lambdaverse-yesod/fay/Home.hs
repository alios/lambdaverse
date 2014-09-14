
module Home where

import           FFI
import           Hexagon
import           HexagonSnap
import           JQuery
import           Language.Fay.Yesod
import           Prelude
import           SharedTypes
import qualified Snap               as Snap

renderGrid :: Hexgrid -> ScreenBox -> Offset -> Snap.Paper -> Fay [Snap.Element]
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

main :: Fay ()
main = do
  putStrLn "Starting up lambdaverse client..."
  registerOnLoad

registerOnLoad :: Fay ()
registerOnLoad = ready $ do
  putStrLn "Initializing ..."

  let g = testGrid
      w = 700
      h = 400
      x = -50
      y = -50
      z = 2
      vb = ScreenBox { viewbox_pos = ScreenCoordinate  { pixel_x = x, pixel_y = y }
                     , viewbox_size = ScreenCoordinate { pixel_x = round $ w * z
                                                       , pixel_y = round $ h * z
                                                       }
                     }

  putStrLn "Creating Rendering Context ..."
  paper <-  Snap.snapSelector ("#map")
--  setPaperViewBox paper ScreenCoordinate { pixel_x = x, pixel_y = y } z
  putStrLn "Creating Map Objects ..."
  es <- renderGrid g vb Even paper
  putStrLn $ "The document has loaded " ++ show (length es) ++ " hexes."
  return ()

