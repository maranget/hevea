################## Configuration parameters
# Compile using ocamlopt, to use ocamlc set TARGET=byte
TARGET=opt
# Install prefix
PREFIX=/usr/local
# Library directory of hevea
LIBDIR=$(PREFIX)/lib/hevea
# Where to install programms
BINDIR=$(PREFIX)/bin
# Install prefix prefix (cf. DESTDIR below)
DESTDIR=
#Where to install hevea.sty
LATEXLIBDIR=$(PREFIX)/lib/hevea
# C preprocessor with proper options
#CPP=cpp -E -P
#Some alternatives...
#Old fashioned Unix
#CPP=/lib/cpp -E -P 
#GCC installed
CPP=gcc -E -P -x c
# reported to work on MacOsX by Georg
#CPP=cc --traditional-cpp -E -P -x c
# reported to work on Mac OS 10.3 (using gcc-3.3) by John T. Hale
#CPP=cc --traditional-cpp -E -P -xassembler-with-cpp
# Repported to work on mac OS X 10.3.3 by Pau Gastin.
CPP=m4 -E -P # This works with mac OS X 10.3.3
############### End of configuration parameters
SUF=
DIR=
HEVEA=./hevea.$(TARGET)
ESPONJA=./esponja.$(TARGET)
OCAMLC=${DIR}ocamlc$(SUF)
OCAMLFLAGS=-g
OCAMLCI=$(OCAMLC)
OCAMLOPT=${DIR}ocamlopt$(SUF)
OCAMLLEX=${DIR}ocamllex$(SUF)
INSTALL=cp
MKDIR=mkdir -p

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
	- $(MKDIR) $(DESTDIR)/$(LATEXLIBDIR)
	$(INSTALL)  hevea.sty $(DESTDIR)/$(LATEXLIBDIR)
	- $(MKDIR) $(DESTDIR)/$(LIBDIR)
	$(INSTALL) cutfoot-fra.html cutfoot-eng.html footer.tex $(DESTDIR)/$(LIBDIR)
	$(INSTALL) contents_motif.gif next_motif.gif previous_motif.gif $(DESTDIR)/$(LIBDIR)
	$(INSTALL) $(ALLLIB) $(DESTDIR)/$(LIBDIR)
	- $(MKDIR)  $(DESTDIR)/$(LIBDIR)/html
	cd html ; $(INSTALL) $(HTMLLIB) $(DESTDIR)/$(LIBDIR)/html
	- $(MKDIR)  $(DESTDIR)/$(LIBDIR)/text
	cd text ; $(INSTALL) $(TEXTLIB) $(DESTDIR)/$(LIBDIR)/text
	- $(MKDIR) $(DESTDIR)/$(LIBDIR)/info
	cd info ; $(INSTALL) $(INFOLIB) $(DESTDIR)/$(LIBDIR)/info



install-opt: install-lib
	$(INSTALL) hevea.opt $(DESTDIR)/$(BINDIR)/hevea
	$(INSTALL) hacha.opt $(DESTDIR)/$(BINDIR)/hacha
	$(INSTALL) esponja.opt $(DESTDIR)/$(BINDIR)/esponja
	$(INSTALL) imagen $(DESTDIR)/$(BINDIR)

install-byte: install-lib
	$(INSTALL) hevea.byte $(DESTDIR)/$(BINDIR)/hevea
	$(INSTALL) hacha.byte $(DESTDIR)/$(BINDIR)/hacha
	$(INSTALL) esponja.byte $(DESTDIR)/$(BINDIR)/esponja
	$(INSTALL) imagen $(DESTDIR)/$(BINDIR)


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
	${OCAMLC} ${OCAMLFLAGS} -pp '${CPP} -DLIBDIR=\"${LIBDIR}\"' -c mylib.ml

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
