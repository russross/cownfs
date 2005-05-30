include Makefile.rules

all:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

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
