CC=ghc
FLAGS=
SRC=Main.hs

all: QueryTest ParseTest Main
.PHONY: all

Main: $(SRC)
	$(CC) $(SRC)

QueryTest:
	$(CC) QueryTest.hs

ParseTest:
	$(CC) ParseTest.hs

clean:
	-@rm $(shell find . -type f -name "*.o")
	-@rm $(shell find . -type f -name "*.hi")
	-@rm $(shell find . -type f -name "*.dyn_*")
.PHONY: clean

purge: clean
	-rm $(shell find . -type f -executable)
.PHONY: purge
