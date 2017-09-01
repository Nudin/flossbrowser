CC=ghc
FLAGS=
SRC=Main.hs Floss/DB.hs Floss/FillDB.hs Floss/Query.hs Floss/Types.hs Str.hs URLtoID.hs

all: Main Browser
.PHONY: all

Browser: Browser.hs
	$(CC) Browser.hs

Main: $(SRC)
	$(CC) $(SRC)

clean:
	-@rm $(shell find . -type f -name "*.o")
	-@rm $(shell find . -type f -name "*.hi")
	-@rm $(shell find . -type f -name "*.dyn_*")
.PHONY: clean

purge: clean
	-rm $(shell find . -type f -executable)
.PHONY: purge
