HTMLGEN=hevea
HTMLCUT=hacha
IMAGEN=imagen
DOCDIR=$(HOME)/public_html/hevea/doc
all: manual.dvi index.html

install:
	-mkdir $(DOCDIR)
	cp *.html *.gif $(DOCDIR)

tmp.tex: manual.tex
	gpic -t < manual.tex > tmp.tex

manual.html: manual.sty tmp.tex
	${HTMLGEN} manual.sty tmp.tex -o manual.html
	${IMAGEN} manual

manual.dvi: tmp.tex
	latex < /dev/null tmp.tex &&\
	latex < /dev/null tmp.tex &&\
	mv tmp.dvi manual.dvi

index.html: manual.html
	$(HTMLCUT)  manual.html

clean:
	rm -f *~ *.aux *.log *.toc

cleanall: clean
	rm -f manual.dvi *.html *.gif  manual.image.tex
