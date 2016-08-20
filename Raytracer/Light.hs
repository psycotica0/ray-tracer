module Raytracer.Light where

import Numeric.LinearAlgebra.Data (Vector, (|>), toList)
import Raytracer.Geometry (Pos, rayTo, Intersectable)
import Raytracer.Camera (fire_ray)
import Data.Maybe (isJust)

data Light a = Light Pos a deriving (Show)
lightColor (Light _ a) = a

computeLighting :: (Intersectable a) => a b -> [Light b] -> Pos -> [b]
computeLighting mesh lights pos = fmap lightColor $ filter (not . visible) lights
  where
  visible (Light lPos _) = isJust $ fire_ray mesh $ pos `rayTo` lPos
