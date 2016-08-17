.PHONY: clean all

all: sdl_test intersection-test image_render

sdl_test: sdl_test.hs Raytracer/Camera.hs Raytracer/Geometry.hs
	ghc sdl_test

intersection-test: intersection-test.hs Raytracer/Camera.hs Raytracer/Geometry.hs
	ghc intersection-test.hs

image_render: image_render.hs Raytracer/Camera.hs Raytracer/Geometry.hs
	ghc -threaded -O2 image_render

test.png: image_render
	./image_render +RTS -N2 -RTS 400 300 1 2 1 -1 -1 0 1

clean:
	$(RM) *.o Raytracer/*.o *.hi Raytracer/*.hi intersection-test sdl_test
