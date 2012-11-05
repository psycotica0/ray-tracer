import Data.Packed.Matrix (fromColumns, asColumn, (@@>))
import Data.Packed.Vector (Vector, (|>), toList)
import Numeric.LinearAlgebra (luSolve, luPacked, pinv, (<>))
import Numeric.LinearAlgebra.Util (cross)
import Data.List (intercalate)
import Data.Ix (range)
import Data.Tuple (swap)
import Data.Function (on)
import Data.Maybe.HT (toMaybe)
import Control.Applicative ((<*>), pure)
import Debug.Trace

valueTrace a = traceShow a a

scalar = (3 |>).repeat
(|*) a = (*) $ scalar a

uncurry3 func (a, b, c) = func a b c

-- This represents a plane of the form p = ax + by + c
data Plane a = Plane (Vector a) (Vector a) (Vector a) deriving (Show)

calc_plane (Plane a b c) n1 n2 = (n1 |* a) + (n2 |* b) + c

-- This represents a ray of the form r = ax + b
data Ray a = Ray (Vector a) (Vector a) deriving (Show)

calc_ray (Ray a b) n1 = (n1 |* a) + b

-- Each of these Vectors represent a point in 3-space
data Triangle a = Triangle (Vector a) (Vector a) (Vector a) deriving (Show)

triangle_to_plane (Triangle p1 p2 p3) = Plane (p2 - p1) (p3 - p1) p1

ray_plane_intersection ray@(Ray r1 r2) (Plane p1 p2 p3) = toMaybe (not $ isInfinite n) $ calc_ray ray n
	where
	mat = fromColumns [p1, p2, -1 |* r1]
	solns = (luSolve.luPacked) mat (asColumn (r2 - p3))
	n = solns @@> (2,0)

ray_triangle_intersect ray triangle = maybe False ((and.([all (>0), (1>).sum]<*>)).pure.toList.transform) intersect
	where
	plane@(Plane v1 v2 p) = triangle_to_plane triangle
	intersect = ray_plane_intersection ray plane
	invmat = pinv $ fromColumns [v1, v2]
	offset = invmat <> p
	transform vec = (invmat <> vec) - offset

ray_intersect_mesh mesh ray = any (ray_triangle_intersect ray) mesh

square v1 v2 p1 = map (uncurry3 Triangle) [(p1, p1 + v1, p1 + v1 + v2), (p1 + v1 + v2, p1 + v2, p1)]

cube v1 v2 v3 p1 = concatMap (uncurry3 square) [(v1, v2, p1), (v2, v3, p1), (v3, v1, p1), (neg v1, neg v2, p2), (neg v2, neg v3, p2), (neg v3, neg v1, p2)]
	where
	p2 = p1 + v1 + v2 + v3
	neg = ((-1) |*)

-- This is a camera. It's got a bunch of crap
-- So, first are settings. They're size of the surface in our units (w h) then resolution of the surface (w h).
-- Then the next two are the position of the camera, and the vector it's pointing in
-- We assume for now that the camera is always level relative to (1, 0, 1) (No roll)
data Camera a = Camera a a Int Int (Vector a) (Vector a)

-- This camera is currently orthographic, rather than perspective
calc_ray_set (Camera width height wres hres pos direction) = rays
	where
	-- This gives me the width axis of my image
	-- It is acheived by a cross product of my looking direction and a vertical axis
	width_axis = cross (3 |>  [0, 1, 0]) direction
	-- This gives me the height axis of my image
	height_axis = cross direction width_axis
	-- This function computes a vector based on progress along a vector
	partial_vector vec steps current_step = (on (/) fromIntegral current_step steps) |* vec
	-- This function computes the position of a ray given its place in the matrix
	ray_pos x y = (partial_vector (width |* width_axis) wres x) + (partial_vector (height |* height_axis) hres y) + pos + (3 |> [-width / 2, -height/2, 0])
	-- Now I compute the associative list of rays using buildMatrix
	rays = map ((Ray direction).(uncurry ray_pos).swap) $ range ((1, 1), (hres, wres))

-- This makes a crude ASCII image
-- I just assume that there are (x * y) items in bools
make_shitty_image x y bools = intercalate "\n" $ takes x $ map pixel bools
	where
	pixel True = '#'
	pixel False = ' '
	takes n list = map fst $ scanl (\acc v -> splitAt v $ snd acc) (splitAt n list) $ replicate (y-1) n

main = putStrLn $ make_shitty_image wres hres $  map (ray_intersect_mesh test_cube) test_rays
	where
	wres = 80
	hres = 20
	test_rays :: [Ray Double]
	test_rays = calc_ray_set $ Camera 4 4 wres hres (3 |> [0,0, -3]) (3 |> [0,0,1])
	test_cube :: [Triangle Double]
	test_cube = cube (3 |> [1, 0, 0]) (3 |> [0, 1, 0]) (3 |> [0, 0, 1]) (3 |> [0, 0, 0])
