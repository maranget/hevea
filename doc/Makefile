LATEX=latex
PDFLATEX=pdflatex
GPIC=gpic
HEVEA=hevea
HEVEAOPTS=-fix -exec xxdate.exe -O
HACHA=hacha
HACHAOPTS=-tocbis
IMAGEN=imagen
HTMLDIR=$(HOME)/public_html/hevea
DOCDIR=$(HTMLDIR)/doc
DISTRIDIR=$(HTMLDIR)/distri

all: index.html

opt:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS) HEVEAOPTS="$(HEVEAOPTS)" HACHAOPTS="$(HACHAOPTS)" HEVEA=../hevea.opt HACHA=../hacha.opt IMAGEN=../imagen all

infoopt:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS) HEVEAOPTS="$(HEVEAOPTS)" HACHAOPTS="$(HACHAOPTS)" ../HEVEA=hevea.opt HACHA=../hacha.opt IMAGEN=imagen.opt info

byte:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS) HEVEAOPTS="$(HEVEAOPTS)" HACHAOPTS="$(HACHAOPTS)" HEVEA="../hevea.byte -fix" HACHA=../hacha.byte IMAGEN=../imagen all

tmp.tex: text.tex
	$(GPIC) -t < text.tex > tmp.tex

manual.html: manual.hva  macros.tex manual.tex version.tex tmp.tex
	${HEVEA} $(HEVEAOPTS) manual.hva manual.tex

manual.dvi: manual.tex macros.tex version.tex tmp.tex
	$(LATEX) < /dev/null manual.tex &&\
	$(LATEX) < /dev/null manual.tex &&\
	makeindex manual.idx &&\
	$(LATEX) < /dev/null manual.tex

manual.ps: manual.dvi
	dvips -o manual.ps manual.dvi

pdfmanual.pdf: pdfmanual.tex macros.tex version.tex tmp.tex
	$(PDFLATEX) < /dev/null pdfmanual.tex &&\
	$(PDFLATEX) < /dev/null pdfmanual.tex &&\
	makeindex pdfmanual.idx &&\
	$(PDFLATEX) < /dev/null pdfmanual.tex

index.html: manual.html
	$(HACHA) $(HACHAOPTS) manual.html

images: manual001.gif

manual001.gif: manual.image.tex
	${IMAGEN} manual

info: manual.inf tmp.tex
	$(HEVEA) $(HEVEAOPTS) -info manual.inf manual.tex -o manual.info

############ Installation stuff
install:
	-mkdir $(DOCDIR)
	cp iso.html symbol.html index.html fddl.html manual[0-9][0-9][0-9].html *.gif $(DOCDIR)
	cp browser.html cutname.html $(DOCDIR)
	cp ../html/urlhref.hva $(DOCDIR)
	cp manual.css $(DOCDIR)

install-distri:
	/bin/rm -rf $(DISTRIDIR)
	mkdir $(DISTRIDIR)
	cp $(FTPDIR)/INDEX  $(FTPDIR)/README $(FTPDIR)/LICENSE $(FTPDIR)/*$(VERSION)* $(DISTRIDIR)
	cp ../hevea.sty $(DISTRIDIR)

############# Release stuff
VERSIONFILE=../version.ml
dateversion::
	 sed  -e "s/^let release_date = .*/let release_date = \"`date +%Y-%m-%d`\"/" $(VERSIONFILE) > /tmp/version.ml && mv /tmp/version.ml $(VERSIONFILE)
	(cd .. ; cvs commit -m version version.ml)


doversion:: $(VERSIONFILE)
	-rm -f version.make version.tex	
	sed -n -e 's/^let real_version = "\(.*\)".*$$/\\def\\heveaversion{\1}/p' $(VERSIONFILE) > version.tex
	sed -n -e 's/^let release_date = "\([0-9\-]*\)".*$$/\\def\\releasedate{\1}/p' $(VERSIONFILE) >> version.tex
	echo "\\newif\\ifdevrelease\\devreleasefalse" >> version.tex
	sed -n -e 's/^let real_version = "\(.*\)".*$$/VERSION=\1/p' $(VERSIONFILE) > version.make
	sed -n -e 's/^let real_version = "\(.\)\.\(.*\)".*$$/RELEASETAG=release-\1-\2/p' $(VERSIONFILE) >> version.make
	sed -n -e '/let real_version = ".*-.*"/s//\\devreleasetrue/p' $(VERSIONFILE) >> version.tex


include version.make

CVSDIR=$(HOME)/CVS
RELEASEDIR=/var/tmp
RELEASENAME=hevea-$(VERSION)
FTPDIR=/home/beaune/moscova0/ftp/hevea
UNSTABLE=

dorelease: doversion
	$(MAKE) $(MFLAGS) byte manual.dvi
#pdfmanual.pdf
# hevea relase
	cd $(RELEASEDIR) ; rm -rf $(RELEASENAME) ;\
	cvs -d $(CVSDIR) export  $(RELEASEOPT) -l htmlgen htmlgen/html htmlgen/text htmlgen/info ;\
	mv htmlgen $(RELEASENAME) ;\
	tar cfo $(RELEASENAME).tar $(RELEASENAME) ;\
	gzip -f --best  $(RELEASENAME).tar
# Doc release
	dvips -o$(RELEASEDIR)/$(RELEASENAME)-manual.ps manual.dvi
	rm -rf $(RELEASEDIR)/$(RELEASENAME)-manual-dvi
	mkdir $(RELEASEDIR)/$(RELEASENAME)-manual-dvi
	cp manual.dvi $(RELEASEDIR)/$(RELEASENAME)-manual-dvi
#	cp pdfmanual.pdf $(RELEASEDIR)/$(RELEASENAME)-manual.pdf
	cp blob.ps cloche.ps int.ps realblob.ps rquote.ps blob2.ps\
home.ps intd.ps round.ps xfd.ps $(RELEASEDIR)/$(RELEASENAME)-manual-dvi
	rm -rf $(RELEASEDIR)/$(RELEASENAME)-manual
	$(MAKE) $(MFLAGS) DOCDIR=$(RELEASEDIR)/$(RELEASENAME)-manual install
	cd $(RELEASEDIR) ;\
	tar cfo $(RELEASENAME)-manual.tar $(RELEASENAME)-manual ;\
	tar cfo $(RELEASENAME)-manual-dvi.tar $(RELEASENAME)-manual-dvi ;\
	gzip -f --best  $(RELEASENAME)-manual-dvi.tar $(RELEASENAME)-manual.ps  $(RELEASENAME)-manual.tar
# Copy to FTPDIR
	-cd $(RELEASEDIR) ; mv $(RELEASENAME)-manual-dvi.tar.gz $(RELEASENAME)-manual.ps.gz  $(RELEASENAME)-manual.pdf $(RELEASENAME)-manual.tar.gz  $(RELEASENAME).tar.gz $(RELEASENAME)/README $(RELEASENAME)/LICENSE $(RELEASENAME)/CHANGES $(FTPDIR)

#Avant, e'diter README, CHANGES, version.ml, commettre, tagger, make doversion
release:
	$(MAKE) $(MFLAGS) dorelease RELEASEOPT="-r $(RELEASETAG)"

testrelease:
	$(MAKE) $(MFLAGS) dorelease RELEASEOPT="-D now"

devrelease:
	$(MAKE) dateversion
	/bin/rm -rf ${FTPDIR}/unstable ; mkdir ${FTPDIR}/unstable
	$(MAKE) $(MFLAGS) dorelease VERSION=`date +%Y.%m.%d` RELEASENAME="hevea-`date +%Y.%m.%d`" RELEASEOPT="-D now" FTPDIR="${FTPDIR}/unstable"
	$(MAKE) install DOCDIR=$(HOME)/public_html/hevea/newdoc

RPMDIR=/usr/src/redhat
dorpm: hevea.spec
	sed -e "s/VERSION/$(VERSION)/g" -e "s|/UNSTABLE|$(UNSTABLE)|g" hevea.spec > $(RPMDIR)/SPECS/$(RELEASENAME).spec
	cp $(FTPDIR)/$(RELEASENAME).tar.gz $(RPMDIR)/SOURCES
	cd $(RPMDIR)/SPECS ; rpm -bb --clean --rmsource $(RELEASENAME).spec
	su $(USER) sh -c "cp $(RPMDIR)/RPMS/i386/$(RELEASENAME)-1.i386.rpm $(FTPDIR)"

devrpm:
	$(MAKE) $(MFLAGS) VERSION=`date +%Y.%m.%d` RELEASENAME="hevea-`date +%Y.%m.%d`"  FTPDIR="${FTPDIR}/unstable" UNSTABLE="/unstable" dorpm

rpm:
	$(MAKE) $(MFLAGS) dorpm
clean:
	rm -f *~ *.aux *.log *.toc *.idx *.ilg *.ind *.haux *.hidx *.hind *.htoc *.hrf

cleanall: clean
	rm -f manual.dvi manual.ps manual[0-9][0-9][0-9].html *_motif.gif \
	manual*.gif manual*.png\
	tmp.* *.image.tex manual.html index.html faq.html\
	 hevea.spec manual.txt manual.info manual.info-[0-9]\
	browser.html cutname.html pdfmanual.pdf

include ../libs.def
RPMLIB=/usr/lib/hevea
hevea.spec: hevea.spec.prelude ../libs.def
	cat hevea.spec.prelude > hevea.spec
	@for i in $(ALLLIB) ; do echo $(RPMLIB)/$$i ; done >> hevea.spec
	@for i in $(HTMLLIB) ; do echo $(RPMLIB)/html/$$i ; done >> hevea.spec
	@for i in $(TEXTLIB) ; do echo $(RPMLIB)/text/$$i ; done >> hevea.spec
	@for i in $(INFOLIB) ; do echo $(RPMLIB)/info/$$i ; done >> hevea.spec