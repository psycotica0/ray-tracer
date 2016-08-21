module Main(main) where

import System.Environment (getArgs)
import Codec.Picture (generateImage, savePngImage, DynamicImage(ImageRGB8), PixelRGB8(PixelRGB8), pixelAt, colorMap, mixWith)
import Numeric.LinearAlgebra.Data ((|>))
import Raytracer.Camera (Camera(Camera), Point(Point), fire_ray, calculate_ray)
import Raytracer.Geometry (cube, square, dist, Collision(Collision), calc_ray)
import Raytracer.Light (Light(Light), computeLighting)
import Data.Monoid ((<>))
import Data.Maybe (isJust)

import Control.Parallel.Strategies (rdeepseq, parMap)
import Control.Concurrent (getNumCapabilities)

help = putStrLn
  "Ray traces an image from a simple scene\n\
  \Run: width height step cx cy cz dx dy dz\n\
  \Where c_ is camera position, d_ is camera direction\n\
  \width and height the size of the image, and step is how many pixels exist between width and height\n\
  \For example, w=200 h=200 step=1 will be a 200x200 image, 200x200x2 will be 100x100, but have the same frame"

test_camera wres hres = Camera 4 3 wres hres (Just 3)
test_cube = cube (PixelRGB8 255 0 255) (3 |> [2, 0, 0]) (3 |> [0, 2, 0]) (3 |> [0, 0, 2]) (3 |> [0, 0, 0])
test_floor = square (PixelRGB8 255 255 255) (3 |> [8,0,0]) (3 |> [0,0,8]) (3 |> [-2,0,-2])
test_mesh = test_cube <> test_floor

test_lights = [
  Light (3|> [-1, 3, 1]) 6 $ PixelRGB8 255 255 255,
  Light (3|> [1, 4, 1]) 5 $ PixelRGB8 0 0 255,
  Light (3|> [6, 0, 0]) 2 $ PixelRGB8 255 0 0,
  Light (3|> [6, 0, 2]) 2 $ PixelRGB8 0 255 0,
  Light (3|> [6, 0, 4]) 2 $ PixelRGB8 0 0 255
  ]

-- This mixes light colours additively
mixColours = foldr (mixWith add) (PixelRGB8 20 20 20)
  where
  add _ c1 c2 = if maxBound - c1 >= c2 then c1 + c2 else maxBound

-- This mixes a pigment with a light subtractively
reflectLight :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
reflectLight surface light = mixWith subColour surface light
  where
  subColour _ cs cl = if cl >= (maxBound - cs) then cl - (maxBound - cs) else minBound

renderPixel camera x y = computePixel $ fire_ray test_mesh ray
  where
  ray = calculate_ray camera $ Point x y
  lights = computeLighting test_mesh test_lights
  computePixel Nothing = PixelRGB8 0 0 0
  computePixel (Just (Collision d c)) = reflectLight c $ mixColours $ lights $ calc_ray ray d

verticalFlip h func camera x y = func camera x (h - y)

parGenerateImage 1 func w h = generateImage func w h
parGenerateImage n func w h = generateImage combine w h
  where
  -- combine rotates through the parallel images putting lines together
  combine x y = pixelAt (images !! (y `mod` n)) x $ y `div` n
  images = parMap rdeepseq generate [0..n-1]
  generate offset = generateImage (\x y -> func x $ y*n + offset) w $ heightFor offset
  -- heightFor offset exists for when the height is not evenly divisible by n
  -- We need to add a couple rows to the earlier images to make up the difference
  heightFor offset = h `div` n + (if offset < h `mod` n then 1 else 0)

main = do
  args <- getArgs
  if length args /= 9 then
    help
  else do
    numCores <- getNumCapabilities
    let (width:height:step:cx:cy:cz:dx:dy:dz:[]) = args
    let w' = (read width) `div` (read step)
    let h' = (read height) `div` (read step)
    let camera = test_camera w' h' (3 |> [read cx, read cy, read cz]) (3 |> [read dx, read dy, read dz])
    let img = parGenerateImage numCores (verticalFlip h' renderPixel camera) w' h'
    savePngImage "test.png" $ ImageRGB8 img
