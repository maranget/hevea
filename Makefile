OBJS=myfiles.cmo location.cmo out.cmo counter.cmo symb.cmo image.cmo subst.cmo save.cmo  aux.cmo latexmacros.cmo  html.cmo entry.cmo index.cmo latexscan.cmo latexmain.cmo

OPTS=$(OBJS:.cmo=.cmx)

all:  htmlgen
opt: htmlgen.opt

htmlgen: ${OBJS}
	ocamlc -o htmlgen ${OBJS}

htmlgen.opt: ${OPTS}
	ocamlopt -o htmlgen.opt ${OPTS}

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
	rm -f subst.ml latexscan.ml aux.ml save.ml entry.ml
	rm -f *.o *.cmi *.cmo *.cmix *.cmx *.o 
	rm -f *~ #*#

depend: latexscan.ml subst.ml save.ml aux.ml entry.ml
	- cp .depend .depend.bak
	ocamldep *.mli *.ml > .depend


include .depend

