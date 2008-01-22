# type in sources here
SOURCES = extension.ml
SUBDIRS = regression 

pa_ostap.cmo: extension.cmo
	$(OCAMLC) -o pa_ostap.cmo -pack $(top_srcdir)/src/bNF3.cmo extension.cmo

include $(top_srcdir)/m4/Makefile.am.vars

CMO -= extension.cmo
CMO += pa_ostap.cmo

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I $(top_srcdir)/src -I $(CAMLP5LIB)

include $(top_srcdir)/m4/Makefile.am.targets

# overwrite some targets
TARGET := $(CMO) $(CMI)

include $(top_srcdir)/m4/Makefile.am.rules
