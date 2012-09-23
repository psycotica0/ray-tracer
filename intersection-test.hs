import qualified Data.Packed.Matrix as M
import qualified Data.Packed.Vector as V
import qualified Numeric.LinearAlgebra as A
import Debug.Trace

approxZero i = i < 1e-6

scalar a = V.buildVector 3 $ const a

data Plane a = Plane (V.Vector a) (V.Vector a) (V.Vector a)

calc_plane (Plane a b c) n1 n2 = (a * (scalar n1)) + (b * (scalar n2)) + c

data Ray a = Ray (V.Vector a) (V.Vector a)

calc_ray (Ray a b) n1 = (a * (scalar n1)) + b

data Triangle a = Triangle (V.Vector a) (V.Vector a) (V.Vector a)

build_triangle (a1, b1, c1) (a2, b2, c2) (a3, b3, c3) = Triangle (3 V.|> [a1, b1, c1]) (3 V.|> [a2,b2,c2]) (3 V.|> [a3,b3,c3])

triangle_to_plane (Triangle p1 p2 p3) = Plane (p2 - p1) (p3 - p1) p1

ray_plane_intersection ray@(Ray r1 r2) plane@(Plane p1 p2 p3) = calc_ray ray n
	where
	mat = M.fromColumns [p1, p2, (scalar (-1)) * r1]
	solns =  A.linearSolve mat (M.asColumn (r2 - p3))
	n = solns M.@@> (2,0)

ray_triangle_intersect ray triangle@(Triangle p1 p2 p3) = 1 > (V.foldVector (+) 0 $ transform point)
	where
	point = ray_plane_intersection ray plane
	plane@(Plane v1 v2 v3) = triangle_to_plane triangle
	invmat = A.pinv $ M.fromColumns [v1, v2]
	offset = invmat A.<> p1
	transform vec = (invmat A.<> vec) - offset

ray_intersect_mesh ray mesh = any (ray_triangle_intersect ray) mesh

uncurry3 func (a, b, c) = func a b c

square v1 v2 p1 = map (uncurry3 Triangle) [(p1, p1 + v1, p1 + v1 + v2), (p1 + v1 + v2, p1 + v2, p1)]

cube v1 v2 v3 p1 = concatMap (uncurry3 square) [(v1, v2, p1), (v2, v3, p1), (v3, v1, p1), (neg v1, neg v2, p2), (neg v2, neg v3, p2), (neg v3, neg v1, p2)]
	where
	p2 = p1 + v1 + v2 + v3
	neg = (*) $ scalar (-1)

main = print $ ray_intersect_mesh test_ray test_cube
	where
	test_ray :: Ray Double
	--test_ray = Ray (V.fromList [1, 1, 0]) (V.fromList [-0.5, -0.5, -1])
	test_ray = Ray (V.fromList [4/3, 1, 2/3]) (V.fromList [0, 0, 0])
	--test_ray = Ray (V.fromList [4/3, 1, 2/3]) (V.fromList [4, 2, 4])
	--test_triangle = build_triangle (3,3,3) (1,1,1) (4,2,0)
	test_cube = cube (3 V.|> [1, 0, 0]) (3 V.|> [0, 1, 0]) (3 V.|> [0, 0, 1]) (3 V.|> [0, 0, 0])
