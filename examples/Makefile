LATEX=latex
HEVEA=hevea
HEVEAOPTS=-fix
IMAGEN=imagen
HACHA=hacha
EXDIR=$(HOME)/public_html/hevea/examples
HTML=a.html pat.html suite.html env.html smile.html amstex.html graphics.html graphicx.html
DVI=$(HTML:.html=.dvi)
TEX=$(HTML:.html=.tex)
TEXT=$(HTML:.html=.txt)
INFO=$(HTML:.html=.info)
# coucou

all: dvi html text info
opt:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS) HEVEAOPTS="$(HEVEAOPTS)" HEVEA=../hevea.opt HACHA=../hacha.opt IMAGEN=../imagen all

byte:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS)  HEVEAOPTS="$(HEVEAOPTS)" HEVEA=../hevea.byte HACHA=../hacha.byte IMAGEN=../imagen all

dvi: $(DVI)


html: $(HTML) index.html
text: $(TEXT)
info: $(INFO)

install:
	/bin/rm -rf $(EXDIR)
	mkdir $(EXDIR)
	cp *.gif $(HTML) index.html tmp*.html conclusion.html benchmarks.html hp.c $(EXDIR)
	-cp $(TEXT) $(EXDIR)
	cp Makefile $(TEX) pat.def env.hva pat.hva smile.hva $(EXDIR)
	cp $(DVI) $(EXDIR)

a.dvi: a.tex
	$(LATEX) < /dev/null a.tex

a.html: a.tex
	$(HEVEA) $(HEVEAOPTS) -francais a.tex

a.txt : a.tex
	$(HEVEA) $(HEVEAOPTS) -text -francais a.tex

a.info : a.tex
	$(HEVEA) $(HEVEAOPTS) -info -francais a.tex

tmp.tex: pat.def pat.tex
	gpic -t < pat.tex > tmp.tex

tmp.aux tmp.bbl pat.dvi: tmp.tex
	$(LATEX) tmp.tex < /dev/null
	bibtex tmp ;
	$(LATEX) tmp.tex < /dev/null
	$(LATEX) tmp.tex < /dev/null
	mv tmp.dvi pat.dvi

tmp.html: tmp.tex pat.hva tmp.bbl 
	$(HEVEA) $(HEVEAOPTS) pat.hva -e pat.def tmp.tex
pat.txt: tmp.tex pat.hva tmp.bbl 
	$(HEVEA) $(HEVEAOPTS) -text pat.hva -e pat.def -o pat.txt tmp.tex
pat.info: tmp.tex pat.hva tmp.bbl 
	$(HEVEA) $(HEVEAOPTS) -info pat.hva -e pat.def -o pat.info tmp.tex

pat.html: tmp.html
	$(HACHA) -o pat.html tmp.html

tenv.tex: env.tex
	gpic -t < env.tex > tenv.tex

env.dvi:tenv.tex 
	$(LATEX) < /dev/null tenv.tex
	mv tenv.dvi env.dvi

env.html: env.hva tenv.tex
	$(HEVEA) $(HEVEAOPTS) env.hva -o env.html tenv.tex
env.txt env.info:


tsmile.tex: smile.tex
	gpic -t < smile.tex > tsmile.tex

smile.dvi:tsmile.tex 
	$(LATEX) < /dev/null tsmile.tex
	mv tsmile.dvi smile.dvi

smile.html: smile.hva tsmile.tex
	$(HEVEA) $(HEVEAOPTS) smile.hva -o smile.html tsmile.tex
smile.txt smile.info:

suite.dvi suite.aux: suite.tex
	$(LATEX) < /dev/null suite.tex
	makeindex suite
	$(LATEX) < /dev/null suite.tex
	$(LATEX) < /dev/null suite.tex

suite.html: suite.tex
	$(HEVEA) $(HEVEAOPTS) suite.tex
suite.txt: suite.tex
	$(HEVEA) $(HEVEAOPTS) -text suite.tex
suite.info: suite.tex
	$(HEVEA) $(HEVEAOPTS) -info suite.tex

amstex.dvi: amstex.tex
	latex < /dev/null amstex.tex

amstex.html: amstex.tex
	$(HEVEA) $(HEVEAOPTS) amstex

amstex.txt: amstex.tex
	$(HEVEA) $(HEVEAOPTS) -text amstex
amstex.info: amstex.tex
	$(HEVEA) $(HEVEAOPTS) -info amstex

HF.ps: HF.ps.gz
	gunzip < HF.ps.gz > HF.ps

graphics.dvi: graphics.tex HF.ps
	latex < /dev/null graphics.tex


graphics.html: graphics.tex HF.ps
	$(HEVEA) $(HEVEAOPTS) graphics
graphics.txt graphics.info:

graphicx.dvi: HF.ps graphicx.tex
	latex < /dev/null graphicx.tex

graphicx.html: graphicx.tex HF.ps
	$(HEVEA) $(HEVEAOPTS) graphicx
graphicx.txt graphicx.info:

index.html: index.tex
	$(HEVEA) $(HEVEAOPTS) index.tex

cleanhtml:
	rm -f $(HTML) tmp.html tmp*.html conclusion.html benchmarks.html
cleantext:
	rm -f $(TEXT)
cleaninfo:
	rm -f *.info

clean cleanall: cleanhtml cleantext cleaninfo
	rm -f *.aux *.log *.dvi *~ *.gif *.bbl tmp.* tenv.* tsmile.* *.ilg *.ind *.idx *.image.tex  *.tid HF.ps *.haux *.hidx
