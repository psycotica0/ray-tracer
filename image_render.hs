module Main(main) where

import System.Environment (getArgs)
import Codec.Picture (generateImage, savePngImage, DynamicImage(ImageRGB8), PixelRGB8(PixelRGB8))
import Numeric.LinearAlgebra.Data ((|>))
import Raytracer.Camera (Camera(Camera), Point(Point), fire_ray, calculate_ray)
import Raytracer.Geometry (cube)

help = putStrLn
  "Ray traces an image from a simple scene\n\
  \Run: width height step cx cy cz dx dy dz\n\
  \Where c_ is camera position, d_ is camera direction\n\
  \width and height the size of the image, and step is how many pixels exist between width and height\n\
  \For example, w=200 h=200 step=1 will be a 200x200 image, 200x200x2 will be 100x100, but have the same frame"

test_camera wres hres = Camera 4 3 wres hres (Just 1)
test_cube = cube (3 |> [2, 0, 0]) (3 |> [0, 2, 0]) (3 |> [0, 0, 2]) (3 |> [0, 0, 0])

renderPixel camera x y = computePixel $ fire_ray test_cube $ calculate_ray camera $ Point x y
  where
  computePixel Nothing = PixelRGB8 0 0 0
  computePixel (Just _) = PixelRGB8 255 0 0

main = do
  args <- getArgs
  if length args /= 9 then
    help
  else do
    let (width:height:step:cx:cy:cz:dx:dy:dz:[]) = args
    let w' = (read width) `div` (read step)
    let h' = (read height) `div` (read step)
    let camera = test_camera w' h' (3 |> [read cx, read cy, read cz]) (3 |> [read dx, read dy, read dz])
    let img = generateImage (renderPixel camera) w' h'
    savePngImage "test.png" $ ImageRGB8 img
