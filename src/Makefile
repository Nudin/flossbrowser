CC=ghc
TMPDIRNAME=build
FLAGS=-Wall -hidir $(TMPDIRNAME) -odir $(TMPDIRNAME)
SRC=CreateDB.hs
.PHONY: purge
.PHONY: all

all: tmpdir main browser

browser: Browser.hs
	$(CC) $(FLAGS) Browser.hs

main: $(SRC)
	$(CC) $(FLAGS) $(SRC)

clean:
	-@rm $(shell find . -type f -name "*.o")
	-@rm $(shell find . -type f -name "*.hi")
	-@rm $(shell find . -type f -name "*.dyn_*")
.PHONY: clean

purge: clean
	-rm $(shell find . -type f -executable)
	-rm flossbrowser.sqlite-shm
	-rm flossbrowser.sqlite-wal
	-rm client_session_key.aes
	-rm -r $(TMPDIRNAME)

tmpdir:
	mkdir -p $(TMPDIRNAME)
