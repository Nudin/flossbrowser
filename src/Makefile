CC=ghc
TMPDIRNAME=build
override FLAGS+=-Wall -hidir $(TMPDIRNAME) -odir $(TMPDIRNAME) -O2
SRC_B=CreateDB.hs
SRC_F=Browser.hs
DEPS=Floss/DB.hs Floss/FillDB.hs Floss/Genlists.hs \
     Floss/Parser.hs Floss/Query.hs Floss/Str.hs Floss/Types.hs


all: tmpdir CreateDB Browser
.PHONY: all

Browser: $(SRC_F) $(DEPS)
	$(CC) $(FLAGS) $(SRC_F)

CreateDB: $(SRC_B) $(DEPS)
	$(CC) $(FLAGS) $(SRC_B)

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
.PHONY: purge

tmpdir: $(TMPDIRNAME)
.PHONY: tmpdir

$(TMPDIRNAME):
	mkdir -p $(TMPDIRNAME)
