CC=ghc
TMPDIRNAME=build
FLAGS=-Wall -hidir $(TMPDIRNAME) -odir $(TMPDIRNAME)
SRC=Main.hs Floss/DB.hs Floss/FillDB.hs Floss/Query.hs Floss/Types.hs Str.hs URLtoID.hs
.PHONY: purge

all: tmpdir Main Browser
.PHONY: all

Browser: Browser.hs
	$(CC) Browser.hs

Main: $(SRC)
	$(CC) $(FLAGS) $(SRC)

clean:
	-@rm $(shell find . -type f -name "*.o")
	-@rm $(shell find . -type f -name "*.hi")
	-@rm $(shell find . -type f -name "*.dyn_*")
.PHONY: clean

purge: clean
	-rm $(shell find . -type f -executable)
	-rm -r $(TMPDIRNAME)

tmpdir:
	mkdir -p $(TMPDIRNAME)
