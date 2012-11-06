.PHONY: clean
intersection-test: intersection-test.hs geometry.hs camera.hs
	ghc $^

clean:
	$(RM) *.o *.hi intersection-test
