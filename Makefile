.PHONY: clean
intersection-test: intersection-test.hs Raytracer/Camera.hs Raytracer/Geometry.hs
	ghc intersection-test.hs

clean:
	$(RM) *.o Raytracer/*.o *.hi Raytracer/*.hi intersection-test
