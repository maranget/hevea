HTMLGEN=hevea
HTMLCUT=htmlcut

all: manual.dvi index.html

manual.html: manual.sty manual.tex
	${HTMLGEN} manual.sty manual.tex

manual.dvi: manual.tex
	latex < /dev/null manual.tex

index.html: manual.html
	$(HTMLCUT)  manual.html

clean:
	rm -f *~ *.aux *.log

cleanall: clean
	rm -f manual.dvi *.html *.gif
