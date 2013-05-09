all: Main

Main: Main.hs
	ghc -o Main -O Main.hs

.PHONY: clean all

clean:
	rm -f Main
	rm -f *.o
	rm -f *.hi
