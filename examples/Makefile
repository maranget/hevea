LATEX=latex
HEVEA=hevea
IMAGEN=imagen
HTMLCUT=hacha
EXDIR=$(HOME)/public_html/hevea/examples
HTML=a.html pat.html suite.html env.html smile.html
DVI=$(HTML:.html=.dvi)
TEX=$(HTML:.html=.tex)
# coucou

all: dvi html images

dvi: $(DVI)


html: $(HTML) index.html

install:
	/bin/rm -rf $(EXDIR)
	mkdir $(EXDIR)
	cp *.gif $(HTML) tmp*.html hp.txt $(EXDIR)
	cp Makefile $(TEX) pat.def pat.sty $(EXDIR)
	cp $(DVI) $(EXDIR)

a.dvi: a.tex
	$(LATEX) < /dev/null a.tex

a.html: a.tex
	$(HEVEA) -v -francais a.tex

tmp.tex: pat.def pat.tex
	gpic -t < pat.tex > tmp.tex

tmp.aux tmp.bbl pat.dvi: tmp.tex
	$(LATEX) tmp.tex < /dev/null
	bibtex tmp ;
	$(LATEX) tmp.tex < /dev/null
	$(LATEX) tmp.tex < /dev/null
	mv tmp.dvi pat.dvi

tmp.html: tmp.tex pat.sty tmp.aux tmp.bbl 
	$(HEVEA) pat.sty -e pat.def tmp.tex

pat.html: tmp.html
	$(HTMLCUT) -o pat.html tmp.html

tenv.tex: env.tex
	gpic -t < env.tex > tenv.tex

env.dvi:tenv.tex 
	$(LATEX) < /dev/null tenv.tex
	mv tenv.dvi env.dvi

env.html: env.sty tenv.tex
	$(HEVEA) env.sty -o env.html tenv.tex

tsmile.tex: smile.tex
	gpic -t < smile.tex > tsmile.tex

smile.dvi:tsmile.tex 
	$(LATEX) < /dev/null tsmile.tex
	mv tsmile.dvi smile.dvi

smile.html: smile.sty tsmile.tex
	$(HEVEA) smile.sty -o smile.html tsmile.tex

suite.dvi suite.aux: suite.tex
	$(LATEX) < /dev/null suite.tex
	makeindex suite
	$(LATEX) < /dev/null suite.tex
	$(LATEX) < /dev/null suite.tex

suite.html: suite.tex suite.aux
	$(HEVEA) suite.tex

index.html: index.tex
	$(HEVEA) index.tex

cleanhtml:
	rm -f $(HTML) tmp.html

clean cleanall: cleanhtml
	rm -f *.aux *.log *.dvi *~ *.gif *.bbl tmp.* tenv.* tsmile.* *.ilg *.ind *.idx *.image.tex *.html *.tid

images:
	$(IMAGEN) tmp
	$(IMAGEN) env
	$(IMAGEN) smile
