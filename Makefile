.PHONY: clean

sdl_test: sdl_test.hs Raytracer/Camera.hs Raytracer/Geometry.hs
	ghc sdl_test

intersection-test: intersection-test.hs Raytracer/Camera.hs Raytracer/Geometry.hs
	ghc intersection-test.hs

clean:
	$(RM) *.o Raytracer/*.o *.hi Raytracer/*.hi intersection-test sdl_test
