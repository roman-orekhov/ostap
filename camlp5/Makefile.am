# type in sources here
if CAMLP5
SOURCES = pa_ostap.ml
SUBDIRS = regression 
else
SOURCES =
SUBDIRS =
endif

include $(top_srcdir)/m4/Makefile.am.vars

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I $(CAMLP5LIB)

include $(top_srcdir)/m4/Makefile.am.targets

# overwrite some targets
TARGET := $(CMO) $(CMI)

include $(top_srcdir)/m4/Makefile.am.rules
