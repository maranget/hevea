LIBDIR=/usr/local/lib/htmlgen
CPP=gcc -E -P -x c
OBJS=parse_opts.cmo myfiles.cmo location.cmo out.cmo counter.cmo symb.cmo image.cmo subst.cmo save.cmo  aux.cmo latexmacros.cmo  html.cmo foot.cmo entry.cmo index.cmo latexscan.cmo latexmain.cmo
OBJSCUT=location.cmo out.cmo cross.cmo cut.cmo cutmain.cmo

OPTS=$(OBJS:.cmo=.cmx)

all:  htmlgen
opt: htmlgen.opt

htmlgen: ${OBJS}
	ocamlc -o htmlgen ${OBJS}

htmlcut: ${OBJSCUT}
	ocamlc -o htmlcut ${OBJSCUT}

htmlgen.opt: ${OPTS}
	ocamlopt -o htmlgen.opt ${OPTS}

myfiles.cmo: myfiles.ml myfiles.cmi
	ocamlc -pp '${CPP} -DLIBDIR=\"${LIBDIR}\"' -c myfiles.ml

myfiles.cmx: myfiles.ml myfiles.cmi
	ocamlopt -pp '${CPP} -DLIBDIR=\"${LIBDIR}\"' -c myfiles.ml

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi .c .mll .cmx

.mll.ml:
	ocamllex $<

.ml.cmx:
	ocamlopt -c $<

.ml.cmo:
	ocamlc -c $<

.mli.cmi:
	ocamlc -c $<

.c:
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f htmlgen htmlgen.opt
	rm -f subst.ml latexscan.ml aux.ml save.ml entry.ml cut.ml
	rm -f *.o *.cmi *.cmo *.cmix *.cmx *.o 
	rm -f *~ #*#

depend: latexscan.ml subst.ml save.ml aux.ml entry.ml cut.ml
	- cp .depend .depend.bak
	ocamldep *.mli *.ml > .depend


include .depend

