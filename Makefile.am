SUBDIRS=src

if CAMLP4
SUBDIRS+=camlp4
endif

if CAMLP5
SUBDIRS+=camlp5
endif

SUBDIRS+=util

.PHONY: doc
doc:
	(cd doc; make doc)
