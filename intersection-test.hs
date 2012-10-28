import qualified Data.Packed.Matrix as M
import qualified Data.Packed.Vector as V
import qualified Numeric.LinearAlgebra as A
import qualified Numeric.LinearAlgebra.Util as U
import qualified Data.List as L
import Data.Ix (range)
import Data.Tuple (swap)
import Data.Function (on)
import Debug.Trace

valueTrace a = traceShow a a

approxZero i = i < 1e-6 && i > -1e-6

scalar a = V.buildVector 3 $ const a
(|*) a b = (scalar a) * b

-- This represents a plane of the form p = ax + by + c
data Plane a = Plane (V.Vector a) (V.Vector a) (V.Vector a) deriving (Show)

calc_plane (Plane a b c) n1 n2 = (n1 |* a) + (n2 |* b) + c

-- This represents a ray of the form r = ax + b
data Ray a = Ray (V.Vector a) (V.Vector a) deriving (Show)

calc_ray (Ray a b) n1 = (n1 |* a) + b

-- Each of these Vectors represent a point in 3-space
data Triangle a = Triangle (V.Vector a) (V.Vector a) (V.Vector a) deriving (Show)

build_triangle (a1, b1, c1) (a2, b2, c2) (a3, b3, c3) = Triangle (3 V.|> [a1, b1, c1]) (3 V.|> [a2,b2,c2]) (3 V.|> [a3,b3,c3])

triangle_to_plane (Triangle p1 p2 p3) = Plane (p2 - p1) (p3 - p1) p1

ray_plane_intersection ray@(Ray r1 r2) plane@(Plane p1 p2 p3) = result undefined
	where
	mat = M.fromColumns [p1, p2, -1 |* r1]
	solns = A.linearSolve mat (M.asColumn (r2 - p3))
	n = solns M.@@> (2,0)
	-- The parameter here is a hack to get around the fact that it doesn't pattern match if there are no parameters
	result fake | approxZero $ A.det mat = Nothing
	result fake = Just $ calc_ray ray n

ray_triangle_intersect ray triangle@(Triangle p1 p2 p3) = result undefined
	where
	intersect = ray_plane_intersection ray plane
	plane@(Plane v1 v2 v3) = triangle_to_plane triangle
	invmat = A.pinv $ M.fromColumns [v1, v2]
	offset = invmat A.<> p1
	transform vec = (invmat A.<> vec) - offset
	result fake | intersect == Nothing = False
	result fake = (V.foldVector (\x acc -> acc && (x > 0)) True transformedPosition) && (1 > (V.foldVector (+) 0 transformedPosition))
		where
		Just point = intersect
		transformedPosition = transform point

ray_intersect_mesh ray mesh = any (ray_triangle_intersect ray) mesh

uncurry3 func (a, b, c) = func a b c

square v1 v2 p1 = map (uncurry3 Triangle) [(p1, p1 + v1, p1 + v1 + v2), (p1 + v1 + v2, p1 + v2, p1)]

cube v1 v2 v3 p1 = concatMap (uncurry3 square) [(v1, v2, p1), (v2, v3, p1), (v3, v1, p1), (neg v1, neg v2, p2), (neg v2, neg v3, p2), (neg v3, neg v1, p2)]
	where
	p2 = p1 + v1 + v2 + v3
	neg = ((-1) |*)

-- This is a camera. It's got a bunch of crap
-- So, first are settings. They're size of the surface in our units (w h) then resolution of the surface (w h).
-- Then the next two are the position of the camera, and the vector it's pointing in
-- We assume for now that the camera is always level relative to (1, 0, 1) (No roll)
data Camera a = Camera a a Int Int (V.Vector a) (V.Vector a)

-- This camera is currently orthographic, rather than perspective
calc_ray_set camera@(Camera width height wres hres pos direction) = rays
	where
	-- This gives me the width axis of my image
	-- It is acheived by a cross product of my looking direction and a vertical axis
	width_axis = U.cross (V.fromList [0, 1, 0]) direction
	-- This gives me the height axis of my image
	height_axis = U.cross direction width_axis
	-- This function computes a vector based on progress along a vector
	partial_vector vec steps current_step = (on (/) fromIntegral current_step steps) |* vec
	-- This function computes the position of a ray given its place in the matrix
	ray_pos x y = (partial_vector (width |* width_axis) wres x) + (partial_vector (height |* height_axis) hres y) + pos + (V.fromList [-width / 2, -height/2, 0])
	-- Now I compute the associative list of rays using buildMatrix
	rays = map (Ray direction) $ map (uncurry ray_pos) $ map swap $ range ((1, 1), (hres, wres))

-- This makes a crude ASCII image
-- I just assume that there are (x * y) items in bools
make_shitty_image x y bools = L.intercalate "\n" $ takes x $ map pixel bools
	where
	pixel True = '#'
	pixel False = ' '
	takes n list = map fst $ scanl (\acc v -> splitAt v $ snd acc) (splitAt n list) $ replicate (y-1) n

main = putStrLn $ make_shitty_image wres hres $  map (\x -> ray_intersect_mesh x test_cube) test_rays
	where
	wres = 80
	hres = 20
	test_rays :: [Ray Double]
	test_rays = calc_ray_set $ Camera 4 4 wres hres (3 V.|> [0,0, -3]) (3 V.|> [0,0,1])
	test_cube :: [Triangle Double]
	test_cube = cube (3 V.|> [1, 0, 0]) (3 V.|> [0, 1, 0]) (3 V.|> [0, 0, 1]) (3 V.|> [0, 0, 0])
