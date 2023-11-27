all:
		cabal build
		cabal install --installdir="." --overwrite-policy=always

.PHONY: clean

clean:
		rm -f latc_llvm
		cabal clean
