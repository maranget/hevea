HTMLGEN=hevea
HTMLCUT=hacha
IMAGEN=imagen
all: manual.dvi index.html

manual.html: manual.sty manual.tex
	${HTMLGEN} manual.sty manual.tex
	${IMAGEN} manual

manual.dvi: manual.tex
	latex < /dev/null manual.tex
	latex < /dev/null manual.tex

index.html: manual.html
	$(HTMLCUT)  manual.html

clean:
	rm -f *~ *.aux *.log *.toc

cleanall: clean
	rm -f manual.dvi *.html *.gif  manual.image.tex
