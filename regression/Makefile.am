# List all tests to run
TESTS = test001 test002 test003 test004 test005 test006 test007 test008 test009 test010 test011 test012 test013 test014 test015 test016 test017 test018 test019 test020 test021 test022 test023 test024 itest001 itest002 itest003 itest004 itest005 itest006 itest007 itest008 itest009 itest010 itest011 itest012 itest013 itest014 itest015 itest016 itest017 

# Set testing script with optional environment valiables
TESTS_ENVIRONMENT = $(srcdir)/test.sh

#PARSER_INCLUDES += -I $(top_srcdir)/src
MODULE_INCLUDES += -I $(top_srcdir)/src

EXTRA_CMA += $(PACKAGE).cma

# OCaml parser call with flags
AM_P4FLAGS := -pp "$(CAMLP4O) $(PARSER_INCLUDES) pa_log.cmo $(LOG)"
#AM_P4FLAGS = -pp "$(CAMLP4R) $(PARSER_INCLUDES)"

# bytecode compiling flags
AM_BFLAGS = -rectypes -w p $(MODULE_INCLUDES) $(DEBUG)
# native (optimized) compiling flags
AM_OFLAGS = -rectypes -w p $(MODULE_INCLUDES) $(DEBUG)

# Clean all generated output
clean-am:
	rm -f $(addsuffix .cmi, $(TESTS)) $(addsuffix .cmo, $(TESTS))
	rm -f $(addsuffix $(EXEEXT), $(TESTS))
	rm -f *.diff *.log

#BFLAGS := -w p

# Rules to build tests
test001: test001.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test002: test002.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test003: test003.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test004: test004.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test005: test005.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test006: test006.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test007: test007.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test008: test008.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test009: test009.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test010: test010.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test011: test011.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test012: test012.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test013: test013.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test014: test014.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test015: test015.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test016: test016.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test017: test017.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test018: test018.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test019: test019.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test020: test020.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test021: test021.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test022: test022.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test023: test023.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

test024: test024.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest001: itest001.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest002: itest002.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest003: itest003.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest004: itest004.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest005: itest005.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest006: itest006.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest007: itest007.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest008: itest008.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest009: itest009.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest010: itest010.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest011: itest011.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest012: itest012.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest013: itest013.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest014: itest014.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest015: itest015.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest016: itest016.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 

itest017: itest017.ml
	$(OCAMLC)  $(BFLAGS) $(EXTRA_CMA) $(P4FLAGS) -o $@$(EXEEXT) $< 
