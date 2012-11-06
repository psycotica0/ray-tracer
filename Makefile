.PHONY: clean
intersection-test: intersection-test.hs
	ghc $^

clean:
	$(RM) *.o Raytracer/*.o *.hi Raytracer/*.hi intersection-test
