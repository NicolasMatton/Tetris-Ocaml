OC=ocamlc
CFLAGS=-c
LFLAGS=-o
EXEC=tetris
PROGRAMS=tetris

all:$(PROGRAMS)

tetramino.cmo: tetramino.ml
	$(OC) $(CFLAGS) $@ $<
	
tetramino.cmi: tetramino.mli
	$(OC) $(CFLAGS) tetramino.mli
	
tetris.cmo: tetramino.cmi tetris.ml
	$(OC) $(CFLAGS) tetris.ml

tetris: tetramino.cmo tetris.cmo
	$(OC) $(LFLAGS) $@ unix.cma graphics.cma $^

clean: $(PROGRAMS)
	rm -f $^ tetramino.cmo tetris.cmo
	
.PHONY:clean all
