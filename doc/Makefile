LATEX=latex
GPIC=gpic
HEVEA=hevea
HACHA=hacha
IMAGEN=imagen
HTMLDIR=$(HOME)/public_html/hevea
DOCDIR=$(HTMLDIR)/doc

all: manual.dvi index.html

tmp.tex: manual.tex version.tex
	$(GPIC) -t < manual.tex > tmp.tex

manual.html: manual.sty tmp.tex
	${HEVEA} manual.sty tmp.tex -o manual.html
	${IMAGEN} manual

manual.dvi: tmp.tex
	$(LATEX) < /dev/null tmp.tex &&\
	$(LATEX) < /dev/null tmp.tex &&\
	$(LATEX) < /dev/null tmp.tex &&\
	mv tmp.dvi manual.dvi

index.html: manual.html
	$(HACHA) manual.html

############ Installation stuff
install:
	-mkdir $(DOCDIR)
	cp iso.html symbol.html index.html manual[0-9][0-9][0-9].html *.gif $(DOCDIR)

############# Release stuff
VERSIONFILE=../version.ml
version:: $(VERSIONFILE)
	- rm -f version.make version.tex
	sed -n -e 's/^let version = "\(.*\)".*$$/\\def\\heveaversion{\1}/p' $(VERSIONFILE) > version.tex
	sed -n -e 's/^let version = "\(.*\)".*$$/VERSION=\1/p' $(VERSIONFILE) > version.make
	sed -n -e 's/^let version = "\(.\)\.\(.*\)".*$$/RELEASETAG=release-\1-\2/p' $(VERSIONFILE) >> version.make


include version.make

CVSDIR=$(HOME)/CVS
RELEASEDIR=/usr/tmp
RELEASENAME=hevea-$(VERSION)
FTPDIR=/home/tobago/para-ftp/hevea

dorelease:
# hevea relase
	cd $(RELEASEDIR) ; rm -rf $(RELEASENAME) ;\
	cvs -d $(CVSDIR) export  $(RELEASEOPT) -l htmlgen ;\
	mv htmlgen $(RELEASENAME) ;\
	tar cfo $(RELEASENAME).tar $(RELEASENAME) ;\
	gzip -f -best  $(RELEASENAME).tar
# Doc release
	dvips -o$(RELEASEDIR)/$(RELEASENAME)-manual.ps manual.dvi
	rm -rf $(RELEASEDIR)/$(RELEASENAME)-manual
	$(MAKE) $(MFLAGS) DOCDIR=$(RELEASEDIR)/$(RELEASENAME)-manual install
	cd $(RELEASEDIR) ; tar cfo $(RELEASENAME)-manual.tar $(RELEASENAME)-manual ;\
	 gzip -f -best $(RELEASENAME)-manual.ps  $(RELEASENAME)-manual.tar
# Copy to FTPDIR
	cd $(RELEASEDIR) ; mv $(RELEASENAME)-manual.ps.gz $(RELEASENAME)-manual.tar.gz  $(RELEASENAME).tar.gz $(RELEASENAME)/README $(RELEASENAME)/LICENSE $(RELEASENAME)/CHANGES $(FTPDIR)

release:
	$(MAKE) $(MFLAGS) dorelease RELEASEOPT="-r $(RELEASETAG)"

testrelease:
	$(MAKE) $(MFLAGS) dorelease RELEASEOPT="-D now"

devrelease:
	/bin/rm -rf ${FTPDIR}/unstable ; mkdir ${FTPDIR}/unstable
	$(MAKE) $(MFLAGS) dorelease RELEASENAME="hevea-`date +%Y-%m-%d`" RELEASEOPT="-D now" FTPDIR="${FTPDIR}/unstable"


clean:
	rm -f *~ *.aux *.log *.toc

cleanall: clean
	rm -f manual.dvi manual[0-9][0-9][0-9].html *_motif.gif \
	manual*.gif tmp.tex manual.image.tex
