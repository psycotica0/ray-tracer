module Raytracer.Light where

import Numeric.LinearAlgebra.Data (Vector, (|>), toList)
import Numeric.LinearAlgebra ((<.>))
import Raytracer.Geometry (Pos, rayTo, Intersectable, Ray(Ray))
import Raytracer.Camera (fire_ray)
import Data.Maybe (isJust)

import Codec.Picture (PixelRGB8(PixelRGB8), colorMap)

-- This has the position of the light, the cutoff range of the light, and the colour
data Light a = Light Pos Double a deriving (Show)

--computeLighting :: (Intersectable a) => a b -> [Light b] -> Pos -> [b]
computeLighting mesh lights pos = fmap attenuate $ filter (not . visible) $ fmap makeRay lights
  where
  makeRay light@(Light lPos _ _) = (pos `rayTo` lPos, light)
  visible (ray, _) = isJust $ fire_ray mesh $ ray

attenuate (Ray ray _, Light _ cutoff a) = if dist > cutoff then PixelRGB8 0 0 0 else blend
  where
  dist = sqrt $ ray <.> ray
  blend = colorMap (\x -> round $ (fromIntegral x) * (cutoff - dist) / cutoff) a
