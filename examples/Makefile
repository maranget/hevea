HTMLGEN=hevea
IMAGEGEN=imagegen
HTMLCUT=htmlcut

all: a.html testsccal.html patbis.html suite.html index.html

a.html: a.tex
	latex < /dev/null a.tex
	$(HTMLGEN) -v -francais a.tex

testsccal.html: mydefs.sty testsccal.tex
	latex < /dev/null testsccal.tex
	$(HTMLGEN) mydefs.sty testsccal.tex

pat.html: pat.tex pat.sty
	gpic -t < pat.tex > tmp.tex
	latex tmp.tex < /dev/null
	bibtex tmp ;
	latex tmp.tex < /dev/null
	latex tmp.tex < /dev/null
	$(HTMLGEN) pat.sty -e pat.def -o pat.html tmp.tex
	$(IMAGEGEN) pat

patbis.html: pat.html
	$(HTMLCUT) -o patbis.html pat.html

suite.html: suite.tex
	latex < /dev/null suite.tex
	$(HTMLGEN) suite.tex

index.html: index.tex
	$(HTMLGEN) index.tex

clean:
	rm -f *.aux *.log *.dvi *.html *~ *.gif *.bbl tmp.*
