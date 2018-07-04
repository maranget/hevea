LATEX=latex
GPIC=gpic
HEVEA=hevea
HEVEAOPTS=-fix -exec xxdate.exe -O
HACHA=hacha
HACHAOPTS=-tocbis
IMAGEN=imagen
HTMLDIR=$(HOME)/public_html/hevea
DOCDIR=$(HTMLDIR)/doc
DISTRIDIR=$(HTMLDIR)/distri

all: doc doc/index.html thai
html: doc/index.html

doc:
	mkdir -p doc
	cp fddl.html doc

docclean::
	/bin/rm -f doc/manual.h{tml,aux,ind,toc} doc/manual.image.tex

opt:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS) HEVEAOPTS="$(HEVEAOPTS)" HACHAOPTS="$(HACHAOPTS)" HEVEA=../hevea.opt HACHA=../hacha.opt IMAGEN=../imagen all

infoopt:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS) HEVEAOPTS="$(HEVEAOPTS)" HACHAOPTS="$(HACHAOPTS)" HEVEA=../hevea.opt HACHA=../hacha.opt IMAGEN=imagen.opt info

byte:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS) HEVEAOPTS="$(HEVEAOPTS)" HACHAOPTS="$(HACHAOPTS)" HEVEA="../hevea.byte" HACHA=../hacha.byte IMAGEN=../imagen all

tmp.tex: text.tex
	$(GPIC) -t < text.tex > tmp.tex

doc/manual.html: doc manual.hva  macros.tex manual.tex version.tex tmp.tex
	$(HEVEA) $(HEVEAOPTS) -o doc/manual.html manual.hva manual.tex

manual.dvi: manual.tex macros.tex version.tex tmp.tex
	$(LATEX) < /dev/null manual.tex &&\
	$(LATEX) < /dev/null manual.tex &&\
	makeindex manual.idx &&\
	$(LATEX) < /dev/null manual.tex

manual.ps: manual.dvi
	dvips -o manual.ps manual.dvi

manual.pdf: manual.ps
	ps2pdf manual.ps

doc/index.html: doc/manual.html
	$(HACHA) $(HACHAOPTS) -o doc/index.html doc/manual.html

info: manual.inf tmp.tex
	$(HEVEA) $(HEVEAOPTS) -info manual.inf manual.tex -o manual.info

thai: doc doc/thaihevea.html

doc/thaihevea.html: thai/thaihevea.ttex
	export TEXINPUTS="thai:" ; $(HEVEA)  $(HEVEAOPTS) -o $@ $<

docclean::
	/bin/rm -f doc/thaihevea.h{aux,ind,toc} doc/thaihevea.image.tex

############# Release stuff
VERSIONFILE=../version.ml

version.tex: $(VERSIONFILE)
	sed -n -e 's/^let real_version = "\(.*\)".*$$/\\def\\heveaversion{\1}/p' $(VERSIONFILE) > version.tex
	sed -n -e 's/^let release_date = "\([0-9\-]*\)".*$$/\\def\\releasedate{\1}/p' $(VERSIONFILE) >> version.tex
	echo '\\newif\\ifdevrelease\\devreleasefalse' >> version.tex
	sed -n -e '/let real_version = ".*+.*"/s//\\devreleasetrue/p' $(VERSIONFILE) >> version.tex

clean:
	rm -f *~ *.aux *.log *.toc *.idx *.ilg *.ind *.haux *.hidx *.hind *.htoc *.hrf

cleanall: clean
	rm -f manual.dvi manual.ps manual.pdf
	rm -f faq.html hevea.spec manual.txt manual.info manual.info-[0-9] pdfmanual.pdf
	/bin/rm -rf doc
	/bin/rm -f tmp.tex version.tex
