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
ESPONJA=./esponja.$(TARGET)
OCAMLC=${DIR}ocamlc
OCAMLFLAGS=-g
OCAMLCI=$(OCAMLC)
OCAMLOPT=${DIR}ocamlopt
OCAMLLEX=${DIR}ocamllex
INSTALL=cp

ONLYESPONJA=emisc.cmo buff.cmo htmllex.cmo htmlparse.cmo htmltext.cmo pp.cmo util.cmo explode.cmo ultra.cmo esponja.cmo

OBJS=version.cmo mysys.cmo stack.cmo location.cmo misc.cmo element.cmo out.cmo table.cmo mylib.cmo parse_opts.cmo  myfiles.cmo symb.cmo save.cmo auxx.cmo  lexstate.cmo subst.cmo latexmacros.cmo counter.cmo noimage.cmo image.cmo length.cmo  get.cmo tabular.cmo htmlCommon.cmo htmlMath.cmo mathML.cmo html.cmo  text.cmo infoRef.cmo info.cmo section.cmo foot.cmo entry.cmo index.cmo colscan.cmo color.cmo hot.cmo package.cmo videoc.cmo verb.cmo latexscan.cmo zyva.cmo $(ONLYESPONJA) latexmain.cmo
OBJSCUT=version.cmo mysys.cmo stack.cmo location.cmo misc.cmo  out.cmo thread.cmo cross.cmo mylib.cmo section.cmo length.cmo save.cmo cut.cmo cutmain.cmo
OBJSESPONJA=mysys.cmo stack.cmo location.cmo $(ONLYESPONJA)  esponjamain.cmo

GENSRC=colscan.ml cut.ml entry.ml get.ml latexscan.ml length.ml save.ml tabular.ml videoc.ml verb.ml infoRef.ml subst.ml htmllex.ml

OPTS=$(OBJS:.cmo=.cmx)
OPTSCUT=$(OBJSCUT:.cmo=.cmx)
OPTSESPONJA=$(OBJSESPONJA:.cmo=.cmx)

include libs.def

all: $(TARGET)
everything: byte opt

install: install-$(TARGET)

opt:
	$(MAKE) $(MFLAGS) TARGET=opt hevea.opt hacha.opt esponja.opt cutfoot-fra.html cutfoot-eng.html

byte:
	$(MAKE) $(MFLAGS) TARGET=byte hevea.byte hacha.byte esponja.byte cutfoot-fra.html cutfoot-eng.html

install-lib:
	-mkdir $(LIBDIR)
	$(INSTALL) hevea.sty cutfoot-fra.html cutfoot-eng.html footer.tex ${LIBDIR}
	$(INSTALL) contents_motif.gif next_motif.gif previous_motif.gif ${LIBDIR}
	$(INSTALL) $(ALLLIB) $(LIBDIR)
	-mkdir  $(LIBDIR)/html
	cd html ; $(INSTALL) $(HTMLLIB) $(LIBDIR)/html
	-mkdir  $(LIBDIR)/text
	cd text ; $(INSTALL) $(TEXTLIB) $(LIBDIR)/text
	-mkdir $(LIBDIR)/info
	cd info ; $(INSTALL) $(INFOLIB) $(LIBDIR)/info



install-opt: install-lib
	$(INSTALL) hevea.opt $(BINDIR)/hevea
	$(INSTALL) hacha.opt $(BINDIR)/hacha
	$(INSTALL) esponja.opt $(BINDIR)/esponja
	$(INSTALL) imagen $(BINDIR)

install-byte: install-lib
	$(INSTALL) hevea.byte $(BINDIR)/hevea
	$(INSTALL) hacha.byte $(BINDIR)/hacha
	$(INSTALL) esponja.byte $(BINDIR)/esponja
	$(INSTALL) imagen $(BINDIR)


hevea.byte: ${OBJS}
	${OCAMLC}  ${OCAMLFLAGS} -o $@ ${OBJS}

hacha.byte: ${OBJSCUT}
	${OCAMLC} ${OCAMLFLAGS} -o $@ ${OBJSCUT}

esponja.byte: ${OBJSESPONJA}
	${OCAMLC} ${OCAMLFLAGS} -o $@ ${OBJSESPONJA}

hevea.opt: ${OPTS}
	${OCAMLOPT} -o $@ ${OPTS}

hacha.opt: ${OPTSCUT}
	${OCAMLOPT} -o $@ ${OPTSCUT}

esponja.opt: ${OPTSESPONJA}
	${OCAMLOPT} -o $@ ${OPTSESPONJA}

mylib.cmo: mylib.ml mylib.cmi
	${OCAMLCI} ${OCAMLFLAGS} -pp '${CPP} -DLIBDIR=\"${LIBDIR}\"' -c mylib.ml

mylib.cmx: mylib.ml mylib.cmi
	${OCAMLOPT} -pp '${CPP} -DLIBDIR=\"${LIBDIR}\"' -c mylib.ml

cutfoot-fra.html: cutfoot.tex html/hevea.hva ${HEVEA}
	HEVEADIR=. ; export HEVEADIR ; ${HEVEA} -francais < cutfoot.tex | ${ESPONJA}> $@

cutfoot-eng.html: cutfoot.tex html/hevea.hva ${HEVEA}
	HEVEADIR=. ; export HEVEADIR ; ${HEVEA} < cutfoot.tex | ${ESPONJA}> $@

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi .c .mll .cmx 

.mll.ml:
	${OCAMLLEX} $<

.ml.cmx:
	${OCAMLOPT} -c $<

.ml.cmo:
	${OCAMLC}  ${OCAMLFLAGS} -c $<

.mli.cmi:
	${OCAMLCI} -c $<

.c:
	$(CC) $(CFLAGS) -o $@ $<

cleanbyte:
	rm -f *.byte
	rm -f *.cmo

clean: cleanbyte
	rm -f *.byte *.opt
	rm -f $(GENSRC)
	rm -f *.o *.cmi *.cmo *.cmx *.o *.ppo *.ppi
	rm -f *~ #*# html/*~ html/#*# text/*~ text/#*# info/*~ info/#*# 
	rm -f cutfoot-fra.html cutfoot-eng.html

depend: $(GENSRC)
	- cp .depend .depend.bak
	ocamldep *.mli *.ml > .depend

videoc.cmi : latexscan.cmi
videoc.cmo : videoc.cmi
include .depend
