module Raytracer.Light where

import Numeric.LinearAlgebra.Data (Vector, (|>), toList)
import Numeric.LinearAlgebra ((<.>))
import Raytracer.Geometry (Pos, rayTo, Intersectable, Ray(Ray))
import Raytracer.Camera (fire_ray)
import Data.Maybe (isJust)

import Codec.Picture (PixelRGB8(PixelRGB8), colorMap)

data Light a = Light Pos a deriving (Show)
lightColor (Light _ a) = a

--computeLighting :: (Intersectable a) => a b -> [Light b] -> Pos -> [b]
computeLighting mesh lights pos = fmap attenuate $ filter (not . visible) $ fmap makeRay lights
  where
  makeRay light@(Light lPos _) = (pos `rayTo` lPos, light)
  visible (ray, _) = isJust $ fire_ray mesh $ ray

attenuate (Ray ray _, Light _ a) = if dist > cutoff then PixelRGB8 0 0 0 else blend
  where
  cutoff = 5
  dist = sqrt $ ray <.> ray
  blend = colorMap (\x -> round $ (fromIntegral x) * (cutoff - dist) / cutoff) a
