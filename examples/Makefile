HTMLGEN=htmlgen.opt
IMAGEGEN=imagegen

a.html: a.tex
	$(HTMLGEN) -v -francais a.tex

testsccal.html: mydefs.sty testsccal.tex
	$(HTMLGEN) mydefs.sty testsccal.tex

pat.html: pat.tex pat.sty
	gpic -t < pat.tex > tmp.tex
	latex tmp.tex < /dev/null
	bibtex tmp ;
	latex tmp.tex < /dev/null
	latex tmp.tex < /dev/null
	$(HTMLGEN) pat.sty -e pat.def -o pat.html tmp.tex
	$(IMAGEGEN) pat

suite.html: suite.tex
	$(HTMLGEN) suite.tex
clean:
	rm -f *.aux *.log *.dvi *.html *~ *.gif *.bbl