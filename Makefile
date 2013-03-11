snip: compile

.PHONY: interp
interp:
	runghc -cpp -DMINIMAL snip.hs all

.PHONY: release
release:
	0release --release snip-local.xml \
		--master-feed-file=snip.xml \
		--archive-dir-public-url="http://gfxmonk.net/dist/0install/snip" \
		--public-scm-repository=test-release \
		--archive-upload-command='copyto ~/Sites/gfxmonk/dist/0install/snip_TEST/' \
		--master-feed-upload-command='copyto ~/Sites/gfxmonk/dist/0install/snip_TEST/'
	mkzero-gfxmonk -p snip.gs -v `cat VERSION` snip-interpreted.xml

0compile:
	0compile setup snip.xml 0compile

compile: 0compile
	cd 0compile && \
		0compile build --clean
	ln -sf 0compile/snip-linux-x86_64/snip

.PHONY: ghci
ghci:
	0path --shell http://gfxmonk.net/dist/0install/haskell-ansi-terminal.xml
	0launch --command=ghci http://gfxmonk.net/dist/0install/ghc.xml

clean:
	rm snip snip.hi snip.o
.PHONY: clean


clean-release:
	rm -f 0inst/release-status
.PHONY: clean-release

