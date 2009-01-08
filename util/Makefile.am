# type in sources here
SOURCES = util.ml
MLI = util.mli

include $(top_srcdir)/m4/Makefile.am.vars

# CMO - all compiled objects
CMO := ../src/pretty.cmo ../src/prioReorderer.cmo ../src/bNF3.cmo ../src/ostap.cmo ../src/msg.cmo ../src/reason.cmo ../src/matcher.cmo ../src/trap.cmo $(SORTED:.ml=.cmo) 
MLI := ../src/pretty.mli ../src/prioReorderer.mli ../src/bNF3.mli ../src/ostap.mli ../src/msg.mli ../src/reason.mli ../src/matcher.mli ../src/trap.mli util.mli

# overwrite some variables
OFLAGS += -w x 
CFLAGS += -w x

# OCaml parser call with flags
AM_P5FLAGS := -pp "$(CAMLP5O) ../camlp5/pa_ostap.cmo $(PARSER_INCLUDES)"

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I ../src

include $(top_srcdir)/m4/Makefile.am.targets

# overwrite some targets

install-data-am: $(TARGET)
if HAVE_OCAMLFIND
	if test "$(SITELIBDIR)" != ""; then \
	    $(mkinstalldirs) $(SITELIBDIR); \
	    $(OCAMLFIND) install -destdir $(SITELIBDIR) $(PACKAGE) $(TARGET); \
	else \
	    $(OCAMLFIND) install $(PACKAGE) $(TARGET); \
	fi
else
	if test "$(SITELIBDIR)" = ""; then \
	    installdir=$(OCAMLLIB)/site-lib/$(PACKAGE); \
	else \
	    installdir=$(SITELIBDIR)/$(PACKAGE); \
	fi; \
	$(mkinstalldirs) $$installdir; \
	for i in $(TARGET); do \
	    $(INSTALL_DATA) $$i $$installdir/$$i; \
	done
endif

#install-exec-am: $(BYTE) $(NATIVE)
#	$(mkinstalldirs) ${bindir}
#	$(INSTALL_PROGRAM) $(BYTE) ${bindir}/$(BYTE)
#	$(INSTALL_PROGRAM) $(NATIVE) ${bindir}/$(PACKAGE)$(EXEEXT)

uninstall-am:
if HAVE_OCAMLFIND
	if test "$(SITELIBDIR)" = ""; then \
	    $(OCAMLFIND) remove $(PACKAGE); \
	else \
	    rm -fR $(SITELIBDIR)/$(PACKAGE); \
	fi
else
	if test "$(SITELIBDIR)" = ""; then \
	    rm -fR $(OCAMLLIB)/site-lib/$(PACKAGE); \
	else \
	    rm -fR $(SITELIBDIR)/$(PACKAGE); \
	fi
endif
#	rm -f ${bindir}/$(BYTE) ${bindir}/$(PACKAGE)$(EXEEXT)

include $(top_srcdir)/m4/Makefile.am.rules
