all:
		cabal build
		cabal install --installdir="." --overwrite-policy=always

.PHONY: clean

clean:
		rm -f latc_llvm
		rm -f latc
		cabal clean
