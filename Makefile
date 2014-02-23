GHCFLAGS=-Wall -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.2

.PHONY: all shell clean doc install

all: report.html doc dist/build/libHSecdsa-$(VERSION).a dist/ecdsa-$(VERSION).tar.gz

install: dist/build/libHSecdsa-$(VERSION).a
	cabal install

shell:
	ghci $(GHCFLAGS)

report.html: ECDSA.hs ECDSA/Util.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/ecdsa/index.html README

README: ecdsa.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\n,s/\\\\\\//\\//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/ecdsa/index.html: dist/setup-config ECDSA.hs ECDSA/Util.hs
	cabal haddock --hyperlink-source

dist/setup-config: ecdsa.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist

dist/build/libHSecdsa-$(VERSION).a: dist/setup-config ECDSA.hs ECDSA/Util.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/ecdsa-$(VERSION).tar.gz: README dist/setup-config ECDSA.hs ECDSA/Util.hs
	cabal check
	cabal sdist
