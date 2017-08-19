CC=ghc
FLAGS=
SRC=

all:
	# $(CC) $(SRC)
	$(CC) QueryTest.hs
	$(CC) ParseTest.hs
	$(CC) FlossDB.hs

clean:
	-@rm $(shell find . -type f -name "*.o")
	-@rm $(shell find . -type f -name "*.hi")
	-@rm $(shell find . -type f -name "*.dyn_*")

purge: clean
	-rm $(shell find . -type f -executable)
