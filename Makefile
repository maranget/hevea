OBJS=location.cmo out.cmo counter.cmo symb.cmo latexmacros.cmo image.cmo subst.cmo save.cmo html.cmo latexscan.cmo latexmain.cmo

OPTS=$(OBJS:.cmo=.cmx)

all:  htmlgen
opt: htmlgen.opt

htmlgen: ${OBJS}
	ocamlc -o htmlgen ${OBJS}

htmlgen.opt: ${OPTS}
	ocamlopt -o htmlgen ${OPTS}

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
	rm -f subst.ml latexscan.ml
	rm -f *.o *.cmi *.cmo *.cmix *.cmx *.o 
	rm -f *~ #*#

depend: latexscan.ml subst.ml save.ml
	- cp .depend .depend.bak
	ocamldep *.mli *.ml > .depend


include .depend

