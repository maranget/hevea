LATEX=latex
HTMLGEN=hevea
IMAGEGEN=imagen
HTMLCUT=hacha
EXDIR=$(HOME)/public_html/hevea/examples
all: a.html testsccal.html patbis.html suite.html index.html
install:
	-mkdir $(EXDIR)
	cp *.gif *.html $(EXDIR)
	cp Makefile a.tex suite.tex pat.tex pat.def pat.sty $(EXDIR)
a.html: a.tex
	$(LATEX) < /dev/null a.tex
	$(HTMLGEN) -v -francais a.tex

testsccal.html: mydefs.sty testsccal.tex
	$(LATEX) < /dev/null testsccal.tex
	$(HTMLGEN) mydefs.sty testsccal.tex

pat.html: pat.tex pat.sty
	gpic -t < pat.tex > tmp.tex
	$(LATEX) tmp.tex < /dev/null
	bibtex tmp ;
	$(LATEX) tmp.tex < /dev/null
	$(LATEX) tmp.tex < /dev/null
	$(HTMLGEN) pat.sty -e pat.def -o pat.html tmp.tex
	$(IMAGEGEN) pat

patbis.html: pat.html
	$(HTMLCUT) -o patbis.html pat.html

suite.html: suite.tex
	$(LATEX) < /dev/null suite.tex
	$(HTMLGEN) suite.tex

index.html: index.tex
	$(HTMLGEN) index.tex

clean cleanall:
	rm -f *.aux *.log *.dvi *.html *~ *.gif *.bbl tmp.* *.ilg *.ind *.idx *.image.tex
