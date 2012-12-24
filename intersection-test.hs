import Data.List (intercalate)
import Data.Packed.Vector ((|>))
import Data.Array (elems, bounds)
import Raytracer.Geometry (cube)
import Raytracer.Camera (Camera(Camera), Point(Point), calculate_rays, fire_rays)

-- This makes a crude ASCII image
make_shitty_image array = intercalate "\n" $ fmap (\list -> "." ++ list ++ ".") $ takes (x+1) $ fmap pixel $ elems array
	where
	(x, y) = let ((Point x0 y0), (Point x1 y1)) = bounds array; in (x1 - x0, y1 - y0)
	pixel (Just _) = '#'
	pixel Nothing = ' '
	takes n list = map fst $ scanl (\acc v -> splitAt v $ snd acc) (splitAt n list) $ replicate y n

main = putStrLn $ make_shitty_image $ fire_rays test_rays test_cube
	where
	wres = 160
	hres = 30
	test_camera = Camera 4 4 wres hres Nothing (3 |> [-3, -6, -3]) (3 |> [1,1,1])
	test_rays = calculate_rays test_camera
	test_cube = cube (3 |> [2, 0, 0]) (3 |> [0, 2, 0]) (3 |> [0, 0, 2]) (3 |> [0, 0, 0])
