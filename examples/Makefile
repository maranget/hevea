LATEX=latex
HEVEA=hevea
BIBHVA=bibhva
HEVEAOPTS=-fix -O -rsz 16
HACHA=hacha
EXDIR=$(HOME)/public_html/hevea/examples
HTML=a.html pat.html suite.html env.html smile.html amstex.html graphics.html graphicx.html verbs.html style-test.html amssymb-test.html boxes.html
HVA=env.hva  pat.hva  smile.hva  st.hva  suite-macros.hva
STY=suite-macros.sty hevea.sty
TEX=$(HTML:.html=.tex)
DVI=a.dvi pat.dvi suite.dvi amstex.dvi verbs.dvi
TEXT=a.txt pat.txt suite.txt env.txt
INFO=$(TEXT:.txt=.info)
PDF=boxes.pdf

all: dvi html text info test-opt

opt:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS) HEVEAOPTS="$(HEVEAOPTS)" HEVEA=../hevea.opt HACHA=../hacha.opt BIBHVA=../bibhva.opt html

byte:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS)  HEVEAOPTS="$(HEVEAOPTS)" HEVEA=../hevea.byte HACHA=../hacha.byte BIBHVA=../bibhva.byte html

dvi: $(DVI)

SUITE=suite.html test.html di.html mp.html lis.html lis2.html\
 acc.html sym.html natbib.html list.html

sym.html:sym.tex st.hva

natbib.html: natbib.tex
	$(HEVEA) $(HEVEAOPTS) natbib.tex
	$(BIBHVA) natbib
	$(HEVEA) $(HEVEAOPTS) natbib.tex

test-opt:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS) HEVEAOPTS="$(HEVEAOPTS)" HEVEA=../hevea.opt HACHA=../hacha.opt BIBHVA=../bibhva.opt test

test: $(SUITE) cart-index.html cart-bis-index.html cart-ter-index.html cbook-index.html

TDIR=$(HOME)/public_html/hevea/examples/test
install-test:
	-mkdir -p $(TDIR)
	cp $(SUITE) $(TDIR)
	mv $(TDIR)/test.html $(TDIR)/index.html
	cp *_motif.gif $(TDIR)
	cp cart-index.html cart.css cart[0-9][0-9][0-9].html  $(TDIR)
	cp cart-bis-index.html cart-bis.css cart-bis[0-9][0-9][0-9].html  $(TDIR)
	cp cart-ter-index.html cart-ter.css cart-ter[0-9][0-9][0-9].html  $(TDIR)
	cp cbook-index.html cbook.css cbook[0-9][0-9][0-9].html  $(TDIR)

clean-test::
	/bin/rm -f $(SUITE) cart.html  cart-ter.html cart-bis.html cbook.html natbib.hbbl

cart-index.html: cart.html
	$(HACHA) -o cart-index.html cart.html

cart-bis-index.html: cart-bis.html
	$(HACHA) -tocbis -o cart-bis-index.html cart-bis.html

cart-ter-index.html: cart-ter.html
	$(HACHA) -tocbis -o cart-ter-index.html cart-ter.html

cbook-index.html: cbook.html
	$(HACHA) -o cbook-index.html cbook.html

clean-test::
	/bin/rm -f cart[0-9][0-9][0-9].html cart.css cart-index.html cart.htoc
	/bin/rm -f cart-bis[0-9][0-9][0-9].html cart-bis.css cart-bis-index.html cart-bis.htoc
	/bin/rm -f cart-ter[0-9][0-9][0-9].html cart-ter.css cart-ter-index.html cart-ter.htoc
	/bin/rm -f cbook[0-9][0-9][0-9].html cbook.css cbook-index.html cbook.htoc
	/bin/rm -f natbib.blg natbib.htoc suite.htoc sym.htoc

clean:: clean-test

byte-test:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS)  HEVEAOPTS="$(TOPTS) $(HEVEAOPTS)" HEVEA=../hevea.byte HACHA=../hacha.byte BIBHVA=../bibhva.byte test

opt-test:
	export HEVEADIR=.. ;\
	$(MAKE) $(MFLAGS)  HEVEAOPTS="$(TOPTS) $(HEVEAOPTS)" HEVEA=../hevea.opt HACHA=../hacha.opt BIBHVA=../bibhva.opt test



html: $(HTML) index.html
text: $(TEXT)
info: $(INFO)

install:
	/bin/rm -rf $(EXDIR)
	mkdir $(EXDIR)
	cp *.gif *.png $(HTML) index.html sym.html tmp.css tmp*.html conclusion.html benchmarks.html hp.c $(EXDIR)
	-cp $(TEXT) $(EXDIR)
	cp Makefile $(TEX) pat.def  $(EXDIR)
	cp $(DVI) $(EXDIR)
	cp $(HVA) $(EXDIR)
	cp $(STY) $(EXDIR)
	$(MAKE) $(MFLAGS) install-test TDIR="$(EXDIR)/test"

a.dvi: a.tex
	$(LATEX) < /dev/null a.tex

a.html: a.tex
	$(HEVEA) $(HEVEAOPTS) a.tex

a.txt : a.tex
	$(HEVEA) $(HEVEAOPTS) -text a.tex

a.info : a.tex
	$(HEVEA) $(HEVEAOPTS) -info a.tex

tmp.tex: pat.def pat.tex
	gpic -t < pat.tex > tmp.tex

hevea.sty: ../hevea.sty
	cp ../hevea.sty .

tmp.bbl pat.dvi: tmp.tex hevea.sty
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

suite.dvi: suite.tex
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
	$(HEVEA) $(HEVEAOPTS) png.hva graphicx
graphicx.txt graphicx.info:

verbs.dvi: verbs.tex
	latex < /dev/null verbs.tex

verbs.html: verbs.tex
	$(HEVEA) $(HEVEAOPTS) verbs

verbs.txt verbs.info:

index.html: index.tex
	$(HEVEA) $(HEVEAOPTS) index.tex

#############
pdf: $(PDF)

##############Clean
cleanhtml:
	rm -f $(HTML) index.html tmp.html tmp*.html conclusion.html benchmarks.html
cleantext:
	rm -f $(TEXT)
cleaninfo:
	rm -f *.info *.info-*
cleanpdf:
	rm -f $(PDF)

clean:: cleanhtml cleantext cleaninfo cleanpdf
	rm -f *.aux *.log *.dvi *~ *.gif *.pnm *.png *.bbl tmp.* tenv.* tsmile.* *.ilg *.ind *.idx *.image.tex  *.tid HF.ps *.haux *.hidx *.hind *.cut contents_motif.svg next_motif.svg previous_motif.svg


.SUFFIXES: .tex .html .pdf

.tex.html:
	$(HEVEA) -fix $(HEVEAOPTS) $<

.tex.pdf:
	rubber -d -f  $<
