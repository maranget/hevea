LATEX=latex
GPIC=gpic
HTMLGEN=hevea
HTMLCUT=hacha
IMAGEN=imagen
DOCDIR=$(HOME)/public_html/hevea/doc
all: manual.dvi index.html

install:
	-mkdir $(DOCDIR)
	cp *.html *.gif $(DOCDIR)

tmp.tex: manual.tex
	$(GPIC) -t < manual.tex > tmp.tex

manual.html: manual.sty tmp.tex
	${HTMLGEN} manual.sty tmp.tex -o manual.html
	${IMAGEN} manual

manual.dvi: tmp.tex
	$(LATEX) < /dev/null tmp.tex &&\
	$(LATEX) < /dev/null tmp.tex &&\
	mv tmp.dvi manual.dvi

index.html: manual.html
	$(HTMLCUT)  manual.html

clean:
	rm -f *~ *.aux *.log *.toc

cleanall: clean
	rm -f manual.dvi *.html *motif.gif  manual*.gif tmp.tex manual.image.tex
