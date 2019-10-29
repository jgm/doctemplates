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

DocTemplates.tmp.hs:
	sed -n '1,//p;n;/^[A-Z]/q;p' src/Text/DocTemplates.hs > $@
	sed -n '/^[A-Z]/,$$p' README.md | pandoc -f markdown-auto_identifiers -t haddock >> $@
	echo "\n-}\n" >> $@
	sed -n '/^module/,$$p' src/Text/DocTemplates.hs >> $@
	stack haddock

haddock: DocTemplates.tmp.hs
	cp $< src/Text/DocTemplates.hs
	rm $<

.PHONY: all clean bench repl prof
