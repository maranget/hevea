OBJS=location.cmo out.cmo counter.cmo symb.cmo latexmacros.cmo image.cmo subst.cmo save.cmo html.cmo latexscan.cmo latexmain.cmo

all:  htmlgen

htmlgen: ${OBJS}
	ocamlc -o htmlgen ${OBJS}

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi .c .mll

.mll.ml:
	ocamllex $<
.ml.cmo:
	ocamlc -c $<

.mli.cmi:
	ocamlc -c $<

.c:
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f htmlgen
	rm -f subst.ml latexscan.ml

	rm -f *.o *.cmi *.cmo *.cmix
	rm -f *~ #*#

depend: latexscan.ml subst.ml save.ml
	- cp .depend .depend.bak
	ocamldep *.mli *.ml > .depend


include .depend

