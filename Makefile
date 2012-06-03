snip: compile

.PHONY: interp
interp:
	runghc -cpp -DMINIMAL snip.hs all

.PHONY: release
release:
	0release-gfxmonk snip.xml \
		--public-scm-repository=origin \
		--archive-upload-command='' \
		--master-feed-upload-command=''

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

