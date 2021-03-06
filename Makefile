
OB_FLAGS = -use-ocamlfind -I src -I util -I camlp5 -I camlparser 

OBV=
ifdef OBV
OB_FLAGS += -verbose 6
endif

OB = ocamlbuild $(OB_FLAGS)

all: sanity native byte camlparser

clean:
		ocamlbuild -clean

sanity:
		ocamlfind query str && \
		ocamlfind query typeutil && \
		[ -x `which camlp5` ] && \
		[ -r `camlp5 -where`/pa_log.cmo ]

extension:
		$(OB) pa_ostap.cmo

byte: extension
		$(OB) ostap.cma

native: extension
		$(OB) ostap.cmxa

camlparser: native
		$(OB) test.native

install: all
		ocamlfind install ostap META _build/ostap.cmi _build/ostap.cma \
		_build/ostap.a _build/ostap.cmxa _build/pa_ostap.cmo \
		_build/ostap.cmo _build/ostap.cmx _build/ostap.o

uninstall:
		ocamlfind remove ostap


#automake helper goals to maintain old build
all-am:
clean-am:
distclean-am:
maintainer-clean-am:
install-am:
uninstall-am:

.PHONY: all clean byte native profile debug sanity extension install uninstall
