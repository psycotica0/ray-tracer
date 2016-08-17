module Raytracer.Geometry where

import Numeric.LinearAlgebra.Data (Vector, (|>), toList, (!))
import Numeric.LinearAlgebra.Data (fromColumns, asColumn, toColumns)
import Data.Maybe.HT (toMaybe)
import Data.Monoid (First(First), Monoid, getFirst, mempty, mappend, mconcat)
import Numeric.LinearAlgebra (luSolve, luPacked, pinv, (<>))
import Control.Applicative ((<*>), pure)
import Control.Monad (join)
import Data.Foldable (find)
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Bool.HT (if')

(|*) = (*).(3|>).repeat

all_of :: [(a -> Bool)] -> a -> Bool
all_of conditions value = and $ conditions <*> (pure value)

-- This represents a ray of the form r = ax + b
data Ray = Ray (Vector Double) (Vector Double) deriving (Show)
calc_ray (Ray a b) n1 = (n1 |* a) + b

class Intersectable a where
	intersection :: Ray -> a -> Maybe Double

ray_plane_intersect :: Ray -> Plane -> (Vector Double)
ray_plane_intersect (Ray r1 r2) (Plane p1 p2 p3) = head.toColumns.(\m -> (luSolve.luPacked) m (asColumn (r2 - p3))) $ fromColumns [p1, p2, -1 |* r1]

-- This represents a plane of the form p = ax + by + c
data Plane = Plane (Vector Double) (Vector Double) (Vector Double) deriving (Show)
calc_plane (Plane a b c) n1 n2 = (n1 |* a) + (n2 |* b) + c

instance Intersectable Plane where
	intersection ray plane = find (all_of [not.isInfinite, (>0)]) (Just n)
		where
		n = (ray_plane_intersect ray plane) ! 2

-- Each of these Vectors represent a point in 3-space
data Triangle = Triangle (Vector Double) (Vector Double) (Vector Double) deriving (Show)

instance Intersectable Triangle where
	intersection ray (Triangle p1 p2 p3) = if' (not $ all_of [all (>0), (1>).sum] [n1,n2]) Nothing $ find (all_of [not.isInfinite, (>0)]) (Just n)
		where
		plane = Plane (p2 - p1) (p3 - p1) p1
		(n1:n2:n:_) = toList $ ray_plane_intersect ray plane

-- We'll call a collection of triangles a mesh...
data Mesh = Mesh [Triangle] deriving (Show)

-- Most of this is just proxying the list of triangles.
instance Monoid Mesh where
	mempty = Mesh []
	mappend (Mesh l1) (Mesh l2) = Mesh $ mappend l1 l2

instance Intersectable Mesh where
	-- We'll return, for the mesh, the first one we hit
	intersection ray (Mesh triangles) = join $ find isJust $ sort $ fmap (intersection ray) triangles

-- This function takes a point and two vectors and generates a mesh representing that square
square v1 v2 p1 = Mesh [Triangle p1 (p1 + v1) (p1 + v1 + v2), Triangle (p1 + v1 + v2) (p1 + v2) p1]

-- This function takes a point and three vectors and generates a mesh representing that cube
cube v1 v2 v3 p1 = mconcat $ fmap (uncurry3 square) [(v1, v2, p1), (v2, v3, p1), (v3, v1, p1), (neg v1, neg v2, p2), (neg v2, neg v3, p2), (neg v3, neg v1, p2)]
	where
	p2 = p1 + v1 + v2 + v3
	neg = ((-1) |*)
	uncurry3 func (a, b, c) = func a b c
