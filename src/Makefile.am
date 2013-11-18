
# type in sources here
SOURCES = pretty.ml prioReorderer.ml bNF3.ml ostream.ml combinators.ml msg.ml trap.ml reason.ml aSCII.ml regexp.ml deterministic.ml matcher.ml
MLI = pretty.mli prioReorderer.mli bNF3.mli ostream.mli combinators.mli msg.mli trap.mli reason.mli aSCII.mli regexp.mli deterministic.mli matcher.mli

include $(top_srcdir)/m4/Makefile.am.vars

# CMO - all compiled objects
CMO := $(SORTED:.ml=.cmo)

# OCaml parser call with flags
AM_P5FLAGS := -pp "$(CAMLP5O) $(PARSER_INCLUDES) pa_checked.cmo pa_log.cmo $(LOG)"
AM_BFLAGS += -w x -rectypes
AM_OFLAGS += -w x -rectypes -for-pack Ostap -cclib '-stack 67108864'
# bigger stack size: -cclib '-stack 33554432' or  -cclib '-link -Wl,--stack,16777216'

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
CMOPK =
CMXPK =
TARGET = $(MLI) $(CMI) $(CMO)

include $(top_srcdir)/m4/Makefile.am.targets

install:

uninstall:

# overwrite some targets
install-data-am: $(TARGET)
	@true
uninstall-am: $(TARGET)
	@true

include $(top_srcdir)/m4/Makefile.am.rules
