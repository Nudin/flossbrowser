CC=ghc
FLAGS=
SRC=QueryTest.hs

all:
	$(CC) $(SRC)

clean:
	-@rm ./*.o
	-@rm ./*.hi
	-@rm ./*.dyn_*

purge: clean
	-rm $(shell find . -type f -executable)
