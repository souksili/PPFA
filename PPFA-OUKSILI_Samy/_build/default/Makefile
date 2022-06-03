.PHONY: all clean native depend

OCB_FLAGS   = -use-ocamlfind -use-menhir -I src -lib unix
OCB = ocamlbuild $(OCB_FLAGS)

all: native

clean:
	$(OCB) -clean

native: depend
	$(OCB) Main.native

depend:
	ocamldep *.ml > .depend

include .depend
