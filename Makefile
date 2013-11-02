all: repl

repl: Main.hs
	rlwrap runhaskell Main.hs
