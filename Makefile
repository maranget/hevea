################## Configuration parameters
# Compile using ocamlopt, to use ocamlc set TARGET=byte
TARGET=opt
# Library directory of hevea
LIBDIR=/usr/local/lib/hevea
# Where to install programms
BINDIR=/usr/local/bin
# A replacement for /lib/cpp
CPP=gcc -E -P -x c 
############### End of configuration parameters

HEVEA=./hevea.$(TARGET)
OCAMLC=ocamlc
OCAMLCI=ocamlc
OCAMLOPT=ocamlopt
OCAMLLEX=ocamllex
INSTALL=cp
OBJS=version.cmo misc.cmo location.cmo parse_opts.cmo mylib.cmo myfiles.cmo  out.cmo counter.cmo symb.cmo image.cmo save.cmo  auxx.cmo latexmacros.cmo  html.cmo section.cmo foot.cmo entry.cmo index.cmo length.cmo colscan.cmo color.cmo latexscan.cmo latexmain.cmo
OBJSCUT=version.cmo misc.cmo location.cmo out.cmo thread.cmo cross.cmo mylib.cmo section.cmo save.cmo cut.cmo cutmain.cmo

OPTS=$(OBJS:.cmo=.cmx)
OPTSCUT=$(OBJSCUT:.cmo=.cmx)

all: $(TARGET)
everything: byte opt

install: install-$(TARGET)

opt: hevea.opt hacha.opt cutfoot-fra.html cutfoot-eng.html
byte:  hevea.byte hacha.byte cutfoot-fra.html cutfoot-eng.html

install-lib:
	-mkdir $(LIBDIR)
	$(INSTALL) hevea.sty article.hva book.hva seminar.hva hevea.hva cutfoot-fra.html cutfoot-eng.html footer.tex ${LIBDIR}
	- ln -s book.hva ${LIBDIR}/report.hva
	$(INSTALL) contents_motif.gif next_motif.gif previous_motif.gif ${LIBDIR}

install-opt: install-lib
	$(INSTALL) hevea.opt $(BINDIR)/hevea
	$(INSTALL) hacha.opt $(BINDIR)/hacha
	$(INSTALL) imagen $(BINDIR)

install-byte: install-lib
	$(INSTALL) hevea.byte $(BINDIR)/hevea
	$(INSTALL) hacha.byte $(BINDIR)/hacha
	$(INSTALL) imagen $(BINDIR)


hevea.byte: ${OBJS}
	${OCAMLC} -o $@ ${OBJS}

hacha.byte: ${OBJSCUT}
	${OCAMLC} -o $@ ${OBJSCUT}

hevea.opt: ${OPTS}
	${OCAMLOPT} -o $@ ${OPTS}

hacha.opt: ${OPTSCUT}
	${OCAMLOPT} -o $@ ${OPTSCUT}

mylib.cmo: mylib.ml mylib.cmi
	${OCAMLCI} -pp '${CPP} -DLIBDIR=\"${LIBDIR}\"' -c mylib.ml

mylib.cmx: mylib.ml mylib.cmi
	${OCAMLOPT} -pp '${CPP} -DLIBDIR=\"${LIBDIR}\"' -c mylib.ml

cutfoot-fra.html: cutfoot.tex hevea.hva ${HEVEA}
	${HEVEA} -francais < cutfoot.tex > $@

cutfoot-eng.html: cutfoot.tex hevea.hva ${HEVEA}
	${HEVEA} < cutfoot.tex > $@

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi .c .mll .cmx 

.mll.ml:
	${OCAMLLEX} $<

.ml.cmx:
	${OCAMLOPT} -c $<

.ml.cmo:
	${OCAMLC} -c $<

.mli.cmi:
	${OCAMLCI} -c $<

.c:
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f *.byte *.opt
	rm -f subst.ml latexscan.ml auxx.ml save.ml entry.ml cut.ml
	rm -f *.o *.cmi *.cmo *.cmix *.cmx *.o *.ppo *.ppi
	rm -f *~ #*#
	rm -f cutfoot-fra.html cutfoot-eng.html

depend: colscan.ml length.ml latexscan.ml subst.ml save.ml auxx.ml entry.ml cut.ml
	- cp .depend .depend.bak
	ocamldep *.mli *.ml > .depend


include .depend
