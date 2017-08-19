CC=ghc
FLAGS=
SRC=Main.hs

all:
	$(CC) $(SRC)
	$(CC) QueryTest.hs
	$(CC) ParseTest.hs

clean:
	-@rm $(shell find . -type f -name "*.o")
	-@rm $(shell find . -type f -name "*.hi")
	-@rm $(shell find . -type f -name "*.dyn_*")

purge: clean
	-rm $(shell find . -type f -executable)
