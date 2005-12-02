OCAMLC=ocamlc -g -warn-error Ae
OCAMLOPT=ocamlopt -warn-error Ae
OCAMLRPCGEN=generator/ocamlrpcgen
OCAMLDEP=ocamldep
OCAMLMKTOP=ocamlmktop
OCAMLINCLUDE=$(HOME)/lib/ocaml
CC=gcc
CCOPTS=-g -Werror -Wall -Wno-unused-variable -malign-double

PACKAGES_BC= unix.cma
PACKAGES_OPT= unix.cmxa

MOUNT_RPC= mount_prot.x
MOUNT_STEMS= $(MOUNT_RPC:.x=_caux)
MOUNT_SOURCES= $(MOUNT_STEMS:=.ml) $(MOUNT_STEMS:=.mli)

NFS3_RPC= nfs3_prot.x
NFS3_STEMS= $(NFS3_RPC:.x=_caux)
NFS3_SOURCES= $(NFS3_STEMS:=.ml) $(NFS3_STEMS:=.mli)

RPC_GENRULE= -caux -csrv
RPC_STEMS= $(MOUNT_STEMS) $(NFS3_STEMS)
RPC_SOURCES= $(MOUNT_SOURCES) $(NFS3_SOURCES)

MODULES= rtypes $(RPC_STEMS) util fh common fhCache readdirCache dirTree lookup mount_api nfs_api nfs_api_debug csrv

TARGETSOPT= cownfsd
OBJECTSOPT= $(MODULES:=.cmx) $(TARGETSOPT:=.cmx)
OBJECTSOPT_O= $(OBJECTSOPT:.cmx=.o)

TARGETSBC= $(TARGETSOPT:=.bc)
SOURCESBC= $(MODULES:=.ml) $(MODULES:=.mli) $(TARGETSBC:.bc=.ml)
OBJECTSBC= $(MODULES:=.cmo) $(TARGETSBC:.bc=.cmo)

.PHONY: clean cleanall all opt native

all:	opt

opt:	$(TARGETSOPT)

bc:     $(TARGETSBC)
	rm -f cownfsd
	mv cownfsd.bc cownfsd

top:    $(MODULES:=.cmo)
	$(OCAMLMKTOP) -o top $(PACKAGES_BC) $(MODULES:=.cmo)

$(OCAMLRPCGEN): generator/cgen.ml generator/main.ml
	make -C generator/

$(MOUNT_SOURCES): $(MOUNT_RPC) $(OCAMLRPCGEN)
	$(OCAMLRPCGEN) $(RPC_GENRULE) $<

$(NFS3_SOURCES): $(NFS3_RPC) $(OCAMLRPCGEN)
	$(OCAMLRPCGEN) $(RPC_GENRULE) $<

COBJS=	nfs3_prot_csrv nfs3_prot_server nfs3_prot_xdr nfs3_prot_svc \
	mount_prot_csrv mount_prot_server mount_prot_xdr mount_prot_svc \
	main_loop

NFSD_OBJ= rtypes $(RPC_STEMS) util fh common fhCache readdirCache dirTree lookup mount_api nfs_api nfs_api_debug csrv cownfsd

cownfsd: $(NFSD_OBJ:=.cmx) $(NFSD_OBJ:=.o) $(COBJS:=.o)
	$(OCAMLOPT) -o $@ $(PACKAGES_OPT) $(COBJS:=.o) $(NFSD_OBJ:=.cmx)

cownfsd.bc: $(NFSD_OBJ:=.cmo) $(COBJS:=.o)
	$(OCAMLC) -custom -o $@ $(PACKAGES_BC) $(COBJS:=.o) $(NFSD_OBJ:=.cmo)

%.cmi: %.mli
	$(OCAMLC) -c $<

%.cmo: %.ml
	$(OCAMLC) -c $< $(PACKAGES)

%.cmx: %.ml
	$(OCAMLOPT) -c $< $(PACKAGES)

%.o: %.c
	$(CC) $(CCOPTS) -c $< -I $(OCAMLINCLUDE)

nfs3_prot_csrv.o: nfs3_prot_csrv.c nfs3_prot_csrv.h nfs3_prot.h

nfs3_prot_server.o: nfs3_prot_server.c nfs3_prot_csrv.h nfs3_prot.h

nfs3_prot_svc.o: nfs3_prot_svc.c nfs3_prot.h

nfs3_prot_xdr.o: nfs3_prot_xdr.c nfs3_prot.h

mount_prot_csrv.o: mount_prot_csrv.h mount_prot.h

mount_prot_server.o: mount_prot_csrv.h mount_prot.h

mount_prot_svc.o: mount_prot.h

mount_prot_xdr.o: mount_prot.h

nfs3_prot.h: nfs3_prot.x
	echo "#include <caml/config.h>" > nfs3_prot.h
	rpcgen -h nfs3_prot.x \
	| grep -v "typedef uint64_t uint64;" \
	| grep -v "typedef int64_t int64;" \
	| grep -v "typedef u_int uint32;" \
	| grep -v "typedef int int32;" \
	>> nfs3_prot.h

nfs3_prot_server.c nfs3_prot_csrv.h nfs3_prot_csrv.c: nfs3_prot.x $(OCAMLRPCGEN)
	$(OCAMLRPCGEN) $(RPC_GENRULE) $<

nfs3_prot_xdr.c: nfs3_prot.x
	rpcgen -c -o nfs3_prot_xdr.c nfs3_prot.x

nfs3_prot_svc.c: nfs3_prot.x
	rpcgen -m -o nfs3_prot_svc.c nfs3_prot.x

mount_prot.h: mount_prot.x
	echo "#include <caml/config.h>" > mount_prot.h
	rpcgen -h mount_prot.x \
	>> mount_prot.h

mount_prot_server.c mount_prot_csrv.h mount_prot_csrv.c: mount_prot.x $(OCAMLRPCGEN)
	$(OCAMLRPCGEN) $(RPC_GENRULE) $<

mount_prot_xdr.c: mount_prot.x
	rpcgen -c -o mount_prot_xdr.c mount_prot.x

mount_prot_svc.c: mount_prot.x
	rpcgen -m -o mount_prot_svc.c mount_prot.x

main_loop.c: main_loop.c.in
	cp main_loop.c.in main_loop.c

main_loop.o: mount_prot.h nfs3_prot.h

clean:
	rm -f $(TARGETSBC) $(TARGETSOPT) top *.cmo *.cmi *.cmx *.o

cleanall: clean
	rm -f $(RPC_SOURCES) *.c *.h
	make -C generator/ clean

depend:
	$(OCAMLDEP) $(SOURCESBC) > depend

include depend
