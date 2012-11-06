module Raytracer.Geometry where

import Data.Packed.Vector (Vector, (|>), toList)
import Data.Packed.Matrix (fromColumns, asColumn, (@@>))
import Data.Maybe.HT (toMaybe)
import Data.Monoid (First(First), Monoid, getFirst, mempty, mappend, mconcat)
import Numeric.LinearAlgebra (luSolve, luPacked, pinv, (<>))
import Control.Applicative ((<*>), pure)

(|*) = (*).(3|>).repeat

-- This represents a ray of the form r = ax + b
data Ray = Ray (Vector Double) (Vector Double) deriving (Show)
calc_ray (Ray a b) n1 = (n1 |* a) + b

class Intersectable a where
	intersection :: Ray -> a -> Maybe (Vector Double)

-- This represents a plane of the form p = ax + by + c
data Plane = Plane (Vector Double) (Vector Double) (Vector Double) deriving (Show)
calc_plane (Plane a b c) n1 n2 = (n1 |* a) + (n2 |* b) + c

instance Intersectable Plane where
	intersection ray@(Ray r1 r2) (Plane p1 p2 p3) = toMaybe (not $ isInfinite n) $ calc_ray ray n
		where
		mat = fromColumns [p1, p2, -1 |* r1]
		solns = (luSolve.luPacked) mat (asColumn (r2 - p3))
		n = solns @@> (2,0)

-- Each of these Vectors represent a point in 3-space
data Triangle = Triangle (Vector Double) (Vector Double) (Vector Double) deriving (Show)

instance Intersectable Triangle where
	intersection ray (Triangle p1 p2 p3) = (intersection ray plane) >>= (\point -> toMaybe (within_triangle point) point)
		where
		plane@(Plane v1 v2 p) = Plane (p2 - p1) (p3 - p1) p1
		intersect = intersection ray plane
		within_triangle = (and.([all (>0), (1>).sum]<*>)).pure.toList.transform
		invmat = pinv $ fromColumns [v1, v2]
		offset = invmat <> p
		transform vec = (invmat <> vec) - offset

-- We'll call a collection of triangles a mesh...
data Mesh = Mesh [Triangle] deriving (Show)

-- Most of this is just proxying the list of triangles.
instance Monoid Mesh where
	mempty = Mesh []
	mappend (Mesh l1) (Mesh l2) = Mesh $ mappend l1 l2

instance Intersectable Mesh where
	-- We'll return, for the mesh, the first one we hit
	-- More correctly, we should actually return the one nearest to the ray's origin (The one the ray would hit first)
	intersection ray (Mesh triangles) = getFirst $ mconcat $ fmap ((First).(intersection ray)) triangles

-- This function takes a point and two vectors and generates a mesh representing that square
square v1 v2 p1 = Mesh [Triangle p1 (p1 + v1) (p1 + v1 + v2), Triangle (p1 + v1 + v2) (p1 + v2) p1]

-- This function takes a point and three vectors and generates a mesh representing that cube
cube v1 v2 v3 p1 = mconcat $ fmap (uncurry3 square) [(v1, v2, p1), (v2, v3, p1), (v3, v1, p1), (neg v1, neg v2, p2), (neg v2, neg v3, p2), (neg v3, neg v1, p2)]
	where
	p2 = p1 + v1 + v2 + v3
	neg = ((-1) |*)
	uncurry3 func (a, b, c) = func a b c
