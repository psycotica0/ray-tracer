import Graphics.UI.SDL.General (withInit, InitFlag(InitVideo, InitEventthread))
import Graphics.UI.SDL.Types (Surface, surfaceGetPixels, surfaceGetPixelFormat, SurfaceFlag(SWSurface))
import Graphics.UI.SDL.Video (setVideoMode, mapRGB)
import qualified Graphics.UI.SDL.Video as V (flip)
import Graphics.UI.SDL.WindowManagement (setCaption)
import Graphics.UI.SDL.Events (Event(Quit, KeyDown), waitEvent)
import Graphics.UI.SDL.Keysym (Keysym(Keysym), SDLKey(SDLK_SPACE))
import Foreign.Ptr (castPtr)
import Data.Array (elems)
import Foreign.Marshal.Array (pokeArray)
import Control.Monad (liftM)

import Control.Applicative ((<*>))

import Data.Maybe (fromJust, isJust)
import Data.List (intersperse)

import Data.Packed.Vector ((|>))

import Raytracer.Geometry (cube)
import Raytracer.Camera (Camera(Camera), calculate_rays, fire_rays)

wres = 400
hres = 300
color_depth = 32

test_camera = Camera 4 3 wres hres (Just 1)
test_positions = map (3 |>) [[1, 1, -1], [2, 1, -1], [2, 2, -1], [2, 2, -2]]
test_directions = map (3 |>) [[0,0,1], [-1, 0, 1], [0,1,1], [0,-1,1]]

test_cube = cube (3 |> [2, 0, 0]) (3 |> [0, 2, 0]) (3 |> [0, 0, 2]) (3 |> [0, 0, 0])

transpose (a, b) (c, d) v = (((v-c) * (b-a)) / (d-c)) + a

compute_pixels surface array = mapM (pixel (minimum just_list, maximum just_list)) list
	where
	list = elems array
	just_list = map fromJust $ filter isJust list
	pixel_format = surfaceGetPixelFormat surface
	pixel range (Just n) = greyscale $ round $ transpose (200, 100) range n
	pixel _ Nothing = greyscale 0
	greyscale a = mapRGB pixel_format a a a

array_to_surface surface array = array_ptr >>= ((flip pokeArray) array)
	where
	array_ptr = liftM castPtr $ surfaceGetPixels surface

wait_to_quit _ = waitEvent >>= handle
	where
	handle Quit = return ()
	handle _ = wait_to_quit ()

wait_to_space _ = waitEvent >>= handle
	where
	handle (KeyDown (Keysym SDLK_SPACE _ _)) = return ()
	handle _ = wait_to_space ()

render _ = do
	screen <- setVideoMode wres hres color_depth [SWSurface]
	pixels <- mapM (compute_pixels screen) $ fmap ((flip fire_rays) test_cube) $ fmap calculate_rays $ fmap test_camera test_positions <*> test_directions
	putStrLn "Images Rendered"
	sequence $ intersperse (V.flip screen >>= wait_to_space) $ map (array_to_surface screen) pixels
	V.flip screen

main = withInit [InitVideo, InitEventthread] $ setCaption "SDL Test" "" >>= render >> putStrLn "Ready To Quit" >>= wait_to_quit

