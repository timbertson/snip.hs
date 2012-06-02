snip: build/snip
.PHONY: snip

build/snip: snip.hs
	mkdir -p build
	cd build && ghc -O -cpp ../snip.hs

snip-local.xml: snip.xml
	0local snip.xml

clean:
	rm snip snip.hi snip.o
.PHONY: clean

0:
	mkzero-gfxmonk -p snip.hs snip.xml
.PHONY: 0

0compile:
	rm -rf ./0compile
	0compile setup snip.xml 0compile
	(cd 0compile && 0compile build)
.PHONY: 0compile

0publish:
	(cd 0compile && 0compile publish 'http://gfxmonk.net/dist/0install/snip/')
	cp 0compile/snip-*.bz2 ~/Sites/gfxmonk/dist/0install/snip/
	0publish --add-from ./0compile/*.xml
.PHONY: 0publish

