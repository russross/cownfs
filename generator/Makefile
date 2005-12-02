# How to invoke compilers and tools:

OCAMLC    = ocamlc -g $(OCAMLC_OPTIONS)
OCAMLOPT  = ocamlopt $(OCAMLOPT_OPTIONS)
OCAMLDEP  = ocamldep $(OCAMLDEP_OPTIONS)
OCAMLYACC = ocamlyacc
OCAMLLEX  = ocamllex

TOOLS_DIR = $(TOP_DIR)/tools

COLLECT_FILES = $(TOOLS_DIR)/collect_files

# Files to remove everywhere by "make clean":

CLEAN_LIST = *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa dll* \
	     *.astamp *.cstamp *.sstamp \
	     *_aux.mli *_aux.ml *_clnt.mli *_clnt.ml *_srv.mli *_srv.ml

# Generic build rules:

.SUFFIXES: .cmo .cmi .cmx .ml .mli .mll .mly .c .o

.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) $<

.c.o:
	$(OCAMLC) -c $<


all:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) ocamlrpcgen

opt:
	true

depend:
	$(OCAMLDEP) *.ml *.mli >depend

generate: parser.ml lexer.ml

lexer.ml: parser.ml

clean:
	rm -f $(CLEAN_LIST) META ocamlrpcgen parser.ml parser.mli lexer.ml

CLEAN: clean
	$(MAKE) -C tests CLEAN

distclean: clean
	$(MAKE) -C tests distclean

install:
	test -n "$(BINDIR)"
	cp ocamlrpcgen $(BINDIR)

uninstall:
	test -n "$(BINDIR)"
	rm -f $(BINDIR)/ocamlrpcgen


PACKAGES = unix

OBJ = rtypes.cmo syntax.cmo options.cmo parser.cmo lexer.cmo rename.cmo \
	generate.cmo cgen.cmo main.cmo

XOBJ = $(OBJ:.cmo=.cmx)

OCAMLC_OPTIONS = -I ..
OCAMLOPT_OPTIONS = -I ..

ocamlrpcgen: $(OBJ)
	$(OCAMLC) -custom -o ocamlrpcgen unix.cma $(OBJ)

include depend

