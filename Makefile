# Compile using ocamlopt, to use ocamlc set TARGET=byte
TARGET=opt
# Library directory of hevea
LIBDIR=/usr/local/lib/hevea
# A replacement for /lib/cpp
CPP=gcc -E -P -x c 
# Where to install programms
BINDIR=/usr/local/bin
HTMLDIR=$(HOME)/public_html/hevea

include version.make

HEVEA=./hevea.$(TARGET)
OCAMLC=ocamlc
OCAMLCI=ocamlc
OCAMLOPT=ocamlopt
OCAMLLEX=ocamllex
INSTALL=cp
OBJS=version.cmo parse_opts.cmo mylib.cmo myfiles.cmo location.cmo out.cmo counter.cmo symb.cmo image.cmo subst.cmo save.cmo  aux.cmo latexmacros.cmo  html.cmo section.cmo foot.cmo entry.cmo index.cmo latexscan.cmo latexmain.cmo
OBJSCUT=version.cmo location.cmo out.cmo thread.cmo cross.cmo mylib.cmo section.cmo save.cmo cut.cmo cutmain.cmo

OPTS=$(OBJS:.cmo=.cmx)
OPTSCUT=$(OBJSCUT:.cmo=.cmx)

all: $(TARGET)
everything: byte opt

install: install-$(TARGET)

opt: hevea.opt hacha.opt cutfoot-fra.html cutfoot-eng.html
byte:  hevea.byte hacha.byte cutfoot-fra.html cutfoot-eng.html

install-lib:
	-mkdir $(LIBDIR)
	$(INSTALL) article.sty book.sty hevea.sty cutfoot-fra.html cutfoot-eng.html footer.tex ${LIBDIR}
	- ln -s book.sty ${LIBDIR}/report.sty
	$(INSTALL) contents_motif.gif next_motif.gif previous_motif.gif ${LIBDIR}

install-opt: install-lib
	$(INSTALL) hevea.opt $(BINDIR)/hevea
	$(INSTALL) hacha.opt $(BINDIR)/hacha
	$(INSTALL) imagen $(BINDIR)

install-byte: install-lib
	$(INSTALL) hevea.byte $(BINDIR)/hevea
	$(INSTALL) hacha.byte $(BINDIR)/hacha
	$(INSTALL) imagen $(BINDIR)

docu:
	cd doc ; $(MAKE) $(MFLAGS)
	cd examples ; $(MAKE) $(MFLAGS)

install-doc:
	$(INSTALL) iso.html symbol.html $(HTMLDIR) 
	cd doc ; $(MAKE) $(MFLAGS) DOCDIR=$(HTMLDIR)/doc install
	-mkdir $(DOCDIR)/examples
	cd examples ; $(MAKE) $(MFLAGS) EXDIR=$(HTMLDIR)/examples install

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

cutmain.cmo: cutmain.ml
	${OCAMLCI} -pp '${CPP} -DLIBDIR=\"${LIBDIR}\"' -c cutmain.ml

cutmain.cmx: cutmain.ml
	${OCAMLOPT} -pp '${CPP} -DLIBDIR=\"${LIBDIR}\"' -c cutmain.ml

cutfoot-fra.html: cutfoot.tex hevea.sty ${HEVEA}
	${HEVEA} -francais < cutfoot.tex > $@

cutfoot-eng.html: cutfoot.tex hevea.sty ${HEVEA}
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
	rm -f subst.ml latexscan.ml aux.ml save.ml entry.ml cut.ml
	rm -f *.o *.cmi *.cmo *.cmix *.cmx *.o *.ppo *.ppi
	rm -f *~ #*#
	rm -f cutfoot-fra.html cutfoot-eng.html

cleanall: clean
	cd doc ; $(MAKE) $(MFLAGS) cleanall
	cd examples ; $(MAKE) $(MFLAGS) cleanall

depend: latexscan.ml subst.ml save.ml aux.ml entry.ml cut.ml
	- cp .depend .depend.bak
	ocamldep *.mli *.ml > .depend


VERSIONFILE=version.ml
version:: $(VERSIONFILE)
	- rm -f version.make version.tex
	sed -n -e 's/^let version = "\(.*\)".*$$/\\def\\heveaversion{\1}/p' $(VERSIONFILE) > version.tex
	sed -n -e 's/^let version = "\(.*\)".*$$/VERSION=\1/p' $(VERSIONFILE) > version.make
	sed -n -e 's/^let version = "\(.\)\.\(.*\)".*$$/RELEASETAG=release-\1-\2/p' $(VERSIONFILE) >> version.make

RELEASEDIR=/usr/tmp
RELEASENAME=hevea-$(VERSION)

release:
	cd $(RELEASEDIR) ; rm -rf $(RELEASENAME)
	cd $(RELEASEDIR); cvs export -r $(RELEASETAG) -l htmlgen
	cd $(RELEASEDIR); mv htmlgen $(RELEASENAME)
	cd $(RELEASEDIR); tar cfo $(RELEASE).tar ./$(RELEASENAME) 
	gzip -best $(RELEASE).tar

include .depend
