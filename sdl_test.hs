import Graphics.UI.SDL.General (withInit, InitFlag(InitVideo, InitEventthread))
import Graphics.UI.SDL.Types (Surface, surfaceGetPixels, surfaceGetPixelFormat, SurfaceFlag(SWSurface))
import Graphics.UI.SDL.Video (setVideoMode, mapRGB)
import qualified Graphics.UI.SDL.Video as V (flip)
import Graphics.UI.SDL.WindowManagement (setCaption)
import Graphics.UI.SDL.Events (Event(Quit), waitEvent)
import Foreign.Ptr (castPtr)
import Data.Array (elems)
import Foreign.Marshal.Array (pokeArray)
import Control.Monad (liftM)

import Data.Packed.Vector ((|>))

import Raytracer.Geometry (cube)
import Raytracer.Camera (Camera(Camera), calculate_rays, fire_rays)

wres = 400
hres = 300
color_depth = 32

test_camera = Camera 4 3 wres hres (3 |> [-3, -6, -3]) (3 |> [1,1,1])
test_rays = calculate_rays test_camera
test_cube = cube (3 |> [2, 0, 0]) (3 |> [0, 2, 0]) (3 |> [0, 0, 2]) (3 |> [0, 0, 0])

transpose (a, b) (c, d) v = (((v-c) * (b-a)) / (d-c)) + a

compute_pixels surface = (mapM pixel).elems
	where
	pixel_format = surfaceGetPixelFormat surface
	pixel (Just n) = greyscale $ round $ transpose (256, 0) (4, 8) n
	pixel Nothing = greyscale 0
	greyscale a = mapRGB pixel_format a a a

array_to_surface surface array = array_ptr >>= ((flip pokeArray) array)
	where
	array_ptr = liftM castPtr $ surfaceGetPixels surface

wait_to_quit _ = waitEvent >>= handle
	where
	handle Quit = return ()
	handle _ = wait_to_quit ()

render _ = do
	screen <- setVideoMode wres hres color_depth [SWSurface]
	pixels <- compute_pixels screen $ fire_rays (calculate_rays test_camera) test_cube
	array_to_surface screen pixels
	V.flip screen

main = withInit [InitVideo, InitEventthread] $ setCaption "SDL Test" "" >>= render >>= wait_to_quit

