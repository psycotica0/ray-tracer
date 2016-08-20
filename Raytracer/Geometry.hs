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
import Data.Function (on)
import Data.Maybe (isJust)
import Data.Bool.HT (if')

(|*) = (*).(3|>).repeat

all_of :: [(a -> Bool)] -> a -> Bool
all_of conditions value = and $ conditions <*> (pure value)

type Pos = Vector Double
type Dir = Vector Double

-- This represents a ray of the form r = ax + b
data Ray = Ray Dir Pos deriving (Show)
calc_ray (Ray a b) n1 = (n1 |* a) + b

-- This computes the ray that goes from the first point to the second
rayTo :: Pos -> Pos -> Ray
p1 `rayTo` p2 = Ray (p2 - p1) p1

-- This wraps a collision with a ray and an object
-- The `a` is what the object was holding, and the double is how far down the
-- ray the collision occured (used to find the "first" collision)
data Collision a = Collision Double a deriving (Show)

dist (Collision d _) = d

instance Eq (Collision a) where
  (==) = (==) `on` dist

instance Ord (Collision a) where
  compare = compare `on` dist

class Intersectable a where
	intersection :: Ray -> a b -> Maybe (Collision b)

ray_plane_intersect :: Ray -> Plane a -> (Vector Double)
ray_plane_intersect (Ray r1 r2) (Plane a p1 p2 p3) = head.toColumns.(\m -> (luSolve.luPacked) m (asColumn (r2 - p3))) $ fromColumns [p1, p2, -1 |* r1]

-- This represents a plane of the form p = ax + by + c
data Plane a = Plane a Dir Dir Pos deriving (Show)
calc_plane (Plane _ a b c) n1 n2 = (n1 |* a) + (n2 |* b) + c

instance Intersectable Plane where
	intersection ray plane@(Plane a _ _ _) = find (all_of [not.isInfinite, (>0)] . dist) $ Just $ Collision n a
		where
		n = (ray_plane_intersect ray plane) ! 2

-- Each of these Vectors represent a point in 3-space
data Triangle a = Triangle a Pos Pos Pos deriving (Show)

instance Intersectable Triangle where
	intersection ray (Triangle a p1 p2 p3) = if' (not $ all_of [all (>0), (1>).sum] [n1,n2]) Nothing $ find (all_of [not.isInfinite, (>0)] . dist) $ Just $ Collision n a
		where
		plane = Plane a (p2 - p1) (p3 - p1) p1
		(n1:n2:n:_) = toList $ ray_plane_intersect ray plane

-- We'll call a collection of triangles a mesh...
data Mesh a = Mesh [Triangle a] deriving (Show)

-- Most of this is just proxying the list of triangles.
instance Monoid (Mesh a) where
	mempty = Mesh []
	mappend (Mesh l1) (Mesh l2) = Mesh $ mappend l1 l2

instance Intersectable Mesh where
	-- We'll return, for the mesh, the first one we hit
	intersection ray (Mesh triangles) = join $ find isJust $ sort $ fmap (intersection ray) triangles

-- This function takes a point and two vectors and generates a mesh representing that square
square a v1 v2 p1 = Mesh [Triangle a p1 (p1 + v1) (p1 + v1 + v2), Triangle a (p1 + v1 + v2) (p1 + v2) p1]

-- This function takes a point and three vectors and generates a mesh representing that cube
cube a v1 v2 v3 p1 = mconcat $ fmap (uncurry3 $ square a) [(v1, v2, p1), (v2, v3, p1), (v3, v1, p1), (neg v1, neg v2, p2), (neg v2, neg v3, p2), (neg v3, neg v1, p2)]
	where
	p2 = p1 + v1 + v2 + v3
	neg = ((-1) |*)
	uncurry3 func (a, b, c) = func a b c
