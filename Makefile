all:
	stack test --test-arguments="${TESTARGS}"

bench:
	stack bench

clean:
	stack clean

prof:
	stack build --profile

repl:
	stack ghci src/Text/DocTemplates.hs --ghc-options=-XOverloadedStrings

DocTemplates.tmp.hs: README.md src/Text/DocTemplates.hs
	sed -n '1,//p;n;/^[A-Z]/q;p' src/Text/DocTemplates.hs > $@
	sed -n '/^[A-Z]/,$$p' README.md | pandoc -f markdown-auto_identifiers -t haddock >> $@
	echo "\n-}\n" >> $@
	sed -n '/^module/,$$p' src/Text/DocTemplates.hs >> $@

example.tmp.hs: README.md
	sed -n '/^``` *haskell/,/^```$$/p' $< | grep -v '^```' > $@

haddock: DocTemplates.tmp.hs example.tmp.hs
	cp $< src/Text/DocTemplates.hs
	rm $<
	stack haddock
	stack runghc -- -isrc example.tmp.hs
	rm example.tmp.hs

.PHONY: all clean bench repl prof
