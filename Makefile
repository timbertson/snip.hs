snip: snip.hs
	ghc -O snip.hs

clean:
	rm snip snip.hi snip.o
.PHONY: clean
