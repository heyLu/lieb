all: repl

repl: Main.hs
	rlwrap runhaskell Main.hs

lieb: Main.hs
	ghc -o lieb Main.hs

clean:
	rm -f Main.{hi,o} lieb
