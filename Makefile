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
OCAMLC=ocamlc -g
OCAMLCI=ocamlc
OCAMLOPT=ocamlopt
OCAMLLEX=ocamllex
INSTALL=cp
OBJS=version.cmo misc.cmo  out.cmo location.cmo table.cmo parse_opts.cmo mylib.cmo myfiles.cmo symb.cmo save.cmo auxx.cmo  lexstate.cmo latexmacros.cmo counter.cmo noimage.cmo image.cmo length.cmo  get.cmo tabular.cmo htmlCommon.cmo htmlMath.cmo mathML.cmo html.cmo  text.cmo infoRef.cmo info.cmo section.cmo foot.cmo entry.cmo index.cmo colscan.cmo color.cmo videoc.cmo verb.cmo latexscan.cmo latexmain.cmo
OBJSCUT=version.cmo misc.cmo  out.cmo location.cmo thread.cmo cross.cmo mylib.cmo section.cmo length.cmo save.cmo cut.cmo cutmain.cmo
GENSRC=auxx.ml colscan.ml cut.ml entry.ml get.ml latexscan.ml length.ml save.ml tabular.ml videoc.ml verb.ml infoRef.ml

OPTS=$(OBJS:.cmo=.cmx) $(OBJMAIN:.cmo=.cmx)
OPTSCUT=$(OBJSCUT:.cmo=.cmx)

HTMLLIB=article.hva book.hva report.hva seminar.hva hevea.hva ams.hva mathaccents.hva multind.hva symb.hva symb-text.hva symb-eng.hva symb-fra.hva symb-mathml.hva symb-ent.html
TEXTLIB=article.hva book.hva report.hva seminar.hva hevea.hva symb.hva
INFOLIB=article.hva book.hva report.hva seminar.hva hevea.hva

all: $(TARGET)
everything: byte opt

install: install-$(TARGET)

opt:
	$(MAKE) $(MFLAGS) TARGET=opt hevea.opt hacha.opt cutfoot-fra.html cutfoot-eng.html

byte:
	$(MAKE) $(MFLAGS) TARGET=byte hevea.byte hacha.byte cutfoot-fra.html cutfoot-eng.html

install-lib:
	-mkdir $(LIBDIR)
	$(INSTALL) hevea.sty cutfoot-fra.html cutfoot-eng.html footer.tex ${LIBDIR}
	$(INSTALL) contents_motif.gif next_motif.gif previous_motif.gif ${LIBDIR}
	-mkdir $(LIBDIR)/html
	cd html ; $(INSTALL) $(HTMLLIB) $(LIBDIR)/html
	-mkdir $(LIBDIR)/text
	cd text ; $(INSTALL) $(TEXTLIB) $(LIBDIR)/text
	-mkdir $(LIBDIR)/info
	cd info ; $(INSTALL) $(INFOLIB) $(LIBDIR)/info



install-opt: install-lib
	$(INSTALL) hevea.opt $(BINDIR)/hevea
	$(INSTALL) hacha.opt $(BINDIR)/hacha
	$(INSTALL) imagen $(BINDIR)

install-byte: install-lib
	$(INSTALL) hevea.byte $(BINDIR)/hevea
	$(INSTALL) hacha.byte $(BINDIR)/hacha
	$(INSTALL) imagen $(BINDIR)


hevea.byte: ${OBJS}
	${OCAMLC} -o $@ ${OBJS} ${OBJMAIN}

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

cutfoot-fra.html: cutfoot.tex html/hevea.hva ${HEVEA}
	HEVEADIR=. ; export HEVEADIR ; ${HEVEA} -francais < cutfoot.tex > $@

cutfoot-eng.html: cutfoot.tex html/hevea.hva ${HEVEA}
	HEVEADIR=. ; export HEVEADIR ; ${HEVEA} < cutfoot.tex > $@

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
	rm -f $(GENSRC)
	rm -f *.o *.cmi *.cmo *.cmix *.cmx *.o *.ppo *.ppi
	rm -f *~ #*# html/*~ html/#*# text/*~ text/#*# info/*~ info/#*# 
	rm -f cutfoot-fra.html cutfoot-eng.html

depend: $(GENSRC)
	- cp .depend .depend.bak
	ocamldep *.mli *.ml > .depend

videoc.cmi : latexscan.cmi
videoc.cmo : videoc.cmi
include .depend
