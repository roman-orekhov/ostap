
# type in sources here
SOURCES = lexer.ml parser.ml
MLI = lexer.mli parser.mli

include $(top_srcdir)/m4/Makefile.am.vars

# CMO - all compiled objects
CMO := $(SORTED:.ml=.cmo)

# OCaml parser call with flags
AM_P5FLAGS := -pp "$(CAMLP5O) $(top_srcdir)/camlp5/pa_ostap.cmo $(PARSER_INCLUDES) pa_log.cmo $(LOG)"
AM_BFLAGS += -w x -rectypes
AM_OFLAGS += -w x -rectypes -cclib '-link -Wl,--stack,67108864'
# bigger stack size: -cclib '-stack 33554432' or  -cclib '-link -Wl,--stack,16777216'

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I ../src -I ../util
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
