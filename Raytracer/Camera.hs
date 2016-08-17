module Raytracer.Camera where

import Numeric.LinearAlgebra.Data (Vector, (|>), toList)
import Raytracer.Geometry (Ray(Ray), (|*), intersection)
import Numeric.LinearAlgebra (cross)
import Data.Array (listArray)
import Data.Monoid (mappend)
import Data.Tuple (swap)
import Data.Ix (range, Ix, index, inRange)
import Data.Function (on)

-- This is a camera. It's got a bunch of crap
-- So, first are settings. They're size of the surface in our units (w h) then resolution of the surface (w h).
-- Then the next two are the position of the camera, and the vector it's pointing in
-- The maybe is Focal Distance. If it's Nothing, then we do an orthographic representation
--   If it's a number, then it's the number of units back that the focal point is.
-- We assume for now that the camera is always level relative to (1, 0, 1) (No roll)
data Camera = Camera Double Double Int Int (Maybe Double) (Vector Double) (Vector Double)

data Point = Point Int Int deriving (Eq, Show)

instance Ord Point where
	compare (Point a b) (Point c d) = mappend (compare b d) (compare a c)

instance Ix Point where
	range ((Point a b), (Point c d)) = (range (b, d)) >>= (\y -> (range (a, c)) >>= (\x -> return $ Point x y))
	index ((Point a b), (Point c d)) (Point e f) = (((c - a) + 1) * (f - b)) + (e - a)
	inRange ((Point a b), (Point c d)) (Point e f) = (inRange (a, c) e) && (inRange (b, d) f)

norm :: Vector Double -> Double
norm = sqrt.sum.(map (^2)).toList

unitize :: Vector Double -> Vector Double
unitize v = (1/(norm v)) |* v

-- This camera is currently orthographic, rather than perspective
calculate_ray (Camera width height wres hres focus pos direction) = (\pos -> Ray (ray_direction pos) pos) . ray_pos
	where
	-- This gives me the width axis of my image
	-- It is acheived by a cross product of my looking direction and a vertical axis
	width_axis = unitize $ cross (3 |>  [0, 1, 0]) direction
	-- This gives me the height axis of my image
	height_axis = unitize $ cross direction width_axis
	-- This function computes a vector based on progress along a vector
	partial_vector vec steps current_step = (on (/) fromIntegral current_step steps) |* vec
	-- This function computes the position of a ray given its place in the matrix
	ray_pos (Point x y) = (partial_vector (width |* width_axis) wres x) + (partial_vector (height |* height_axis) hres y) + pos + (3 |> [-width / 2, -height/2, 0])
	-- This function takes in the point and returns the direction of the ray at that point
	-- If it's perspective it involves subtracting from the focal point. If it's orthographic it's just always direction.
	ray_direction = maybe (const direction) (\len -> (flip (-)) $ pos - (len |* direction)) focus

-- This function, finally, generates an array of wres and hres full of the proper ray at each point
calculate_rays camera = build_array (calculate_ray camera) (Point 1 1, Point wres hres)
	where
	(Camera _ _ wres hres _ _ _) = camera
	-- This function takes a function expecting (x,y) and bounds and builds an array by calling the function at each point
	build_array func bounds = listArray bounds $ map func $ range bounds

fire_ray mesh ray = intersection ray mesh
fire_rays rays mesh = fmap (fire_ray mesh) rays
