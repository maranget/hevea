LATEX=latex
HEVEA=hevea
HEVEAOPTS=-fix
IMAGEN=imagen
HACHA=hacha
EXDIR=$(HOME)/public_html/hevea/examples
HTML=a.html pat.html suite.html env.html smile.html amstex.html graphics.html graphicx.html
DVI=$(HTML:.html=.dvi)
TEX=$(HTML:.html=.tex)
# coucou

all: dvi html
opt:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS) HEVEAOPTS="$(HEVEAOPTS)" HEVEA=../hevea.opt HACHA=../hacha.opt IMAGEN=../imagen all

byte:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS)  HEVEAOPTS="$(HEVEAOPTS)" HEVEA=../hevea.byte HACHA=../hacha.byte IMAGEN=../imagen html

dvi: $(DVI)


html: $(HTML) index.html

install:
	/bin/rm -rf $(EXDIR)
	mkdir $(EXDIR)
	cp *.gif $(HTML) index.html tmp*.html hp.c $(EXDIR)
	cp Makefile $(TEX) pat.def env.hva pat.hva smile.hva $(EXDIR)
	cp $(DVI) $(EXDIR)

a.dvi: a.tex
	$(LATEX) < /dev/null a.tex

a.html: a.tex
	$(HEVEA) $(HEVEAOPTS) -francais a.tex

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


pat.html: tmp.html
	$(HACHA) -o pat.html tmp.html

tenv.tex: env.tex
	gpic -t < env.tex > tenv.tex

env.dvi:tenv.tex 
	$(LATEX) < /dev/null tenv.tex
	mv tenv.dvi env.dvi

env.html: env.hva tenv.tex
	$(HEVEA) $(HEVEAOPTS) env.hva -o env.html tenv.tex


tsmile.tex: smile.tex
	gpic -t < smile.tex > tsmile.tex

smile.dvi:tsmile.tex 
	$(LATEX) < /dev/null tsmile.tex
	mv tsmile.dvi smile.dvi

smile.html: smile.hva tsmile.tex
	$(HEVEA) $(HEVEAOPTS) smile.hva -o smile.html tsmile.tex

suite.dvi suite.aux: suite.tex
	$(LATEX) < /dev/null suite.tex
	makeindex suite
	$(LATEX) < /dev/null suite.tex
	$(LATEX) < /dev/null suite.tex

suite.html: suite.tex
	$(HEVEA) $(HEVEAOPTS) suite.tex

amstex.dvi: amstex.tex
	latex < /dev/null amstex.tex

amstex.html: amstex.tex
	$(HEVEA) $(HEVEAOPTS) ams.hva amstex

HF.ps: HF.ps.gz
	gunzip < HF.ps.gz > HF.ps

graphics.dvi: graphics.tex HF.ps
	latex < /dev/null graphics.tex


graphics.html: graphics.tex HF.ps
	$(HEVEA) $(HEVEAOPTS) graphics

graphicx.dvi: HF.ps graphicx.tex
	latex < /dev/null graphicx.tex

graphicx.html: graphicx.tex HF.ps
	$(HEVEA) $(HEVEAOPTS) graphicx

index.html: index.tex
	$(HEVEA) $(HEVEAOPTS) index.tex

cleanhtml:
	rm -f $(HTML) tmp.html *.txt *.info

clean cleanall: cleanhtml
	rm -f *.aux *.log *.dvi *~ *.gif *.bbl tmp.* tenv.* tsmile.* *.ilg *.ind *.idx *.image.tex *.html *.tid HF.ps.gz

images:
	$(IMAGEN) tmp
	$(IMAGEN) env
	$(IMAGEN) smile
