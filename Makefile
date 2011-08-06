TEST_PACKAGES = -package SDL -package SDL-image -package containers -package random
GHC = ghc

test: main.lhs
	$(GHC) -o $@ $(TEST_PACKAGES) $<

clean:
	rm -f *.hi *.o test

