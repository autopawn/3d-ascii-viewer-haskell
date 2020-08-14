all: bin/viewer

bin/viewer:
	mkdir -p bin
	ghc *.hs -o bin/viewer
